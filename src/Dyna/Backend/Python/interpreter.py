#!/usr/bin/env python

"""

This error message is unhelpful

    :-dispos_def dyna.
    :-ruleix 27.
    rewrite("VP", "V", "NP") -= 100.

    FATAL: Encountered error in input program:
     Parser error
     /tmp/tmp.dyna:1:1: error: expected: end of input
     :-dispos_def dyna.

MISC
====

 - TODO: filter and bulk loader

 - TODO: make sure interpreter uses the right exceptions. The codegen catches a
   few things -- I think assertionerror is one them... we should probably do
   whatever this is doing with a custom exception.

 - TODO: mode planning failures are slient.

      timv: I think this is a job for @nwf

 - TODO: create an Interpreter object to hold state.

 - TODO: deleting a rule: (1) remove update handlers (2) run initializers in
   delete mode (3) remove initializers.

 - TODO: hooks from introspection, eval, and prioritization.

     What's the default prioritization?

 - TODO: Term values should only be aggregated with ``=`` or ``:=`` maybe even
   ``set=``. We should disallow ``a += &b.``

     Equals aggregation only one value allowed, mult. >0 on single value. The
     following program has one FP of `c` end `e` are mutually exclusive.

       a = b for c
       a = d for e

     This might not be the case during computation -- this is the same as the
     error problem.

 - TODO: doc tests for Dyna code!

 - TODO: repl needs to pass parser a rule index pragma to start from.

      blocked: nwf will tell me what bits of parser state to send back to him.

 - TODO: Numeric precision is an issue with BAggregators.

         timv: Are we sure we have this bug? or possible that we want to handle
         it in an adhoc fashion?

     a[0.1] += 1
     a[0.1 + eps] -= 1

   Approaches:

    - arbitrary precision arithmetic

    - approximate deletion ("buckets"), find the nearest neighbor and delete it.

    - hybrid: maintain streaming sum and the bag check periodically for quality
      and null.

    - numeric approximations, stream folding (fails to get null)

    - delete the hyperedge: not sure this is perfect because hyperedges aren't
      named with numeric values of variables.


What is null?
=============

 Consider the following two similar programs

  1) a += b + c.

  2) a += b.
     a += c.

 These programs have different meanings! We can demonstrate by adding evidence.

 What should `a += null` and `a += null + 1.` do?


Warnings/lint checking
======================

 - Catch typos! Warn the user if they write a predicate that is not defined on
   the LHS of a rule and it's not quoted (i.e. not some new piece of structure).


REPL
====

 - TODO: (Aggregator conflicts)

   We throw and AggregatorConflict exception if newly loaded code tries to
   overwrite an aggregator in `_agg_decl`.

   However, we need to make sure that we don't load subsequent code pertaining
   to this rule that we should reject altogether.

   timv: At the moment I believe we're safe because `_agg_decl` is set before
   any of the registers (i.e. it's at the top of the generated code). This
   obviously isn't the best way to do this, but we're going to have to overhaul
   this entire infrastructure soon to handle rule-retraction.. So we can fix
   this later.

"""

from __future__ import division
import re, os, sys
from collections import defaultdict
from functools import partial
from argparse import ArgumentParser

from StringIO import StringIO
import webbrowser

from utils import ip, red, green, blue, magenta, yellow, dynahome, \
    notimplemented, prioritydict
from defn import aggregator


class AggregatorConflict(Exception):
    def __init__(self, key, expected, got):
        msg = "Aggregator conflict %r was %r trying to set to %r." \
            % (key, expected, got)
        super(AggregatorConflict, self).__init__(msg)


class DynaInitializerException(Exception):
    def __init__(self, exception, init):
        msg = '%r in ininitializer for rule\n  %s\n        %s' % \
            (exception,
             init.dyna_attrs['Span'],
             init.dyna_attrs['rule'])
        super(DynaInitializerException, self).__init__(msg)


# TODO: as soon as we have safe names for these things we can get rid of this.
class chart_indirect(dict):
    def __missing__(self, key):
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(name = key, arity = arity)
        return c


# when a new rule comes along it puts a string in the following dictionary
class aggregator_declaration(object):
    def __init__(self):
        self.map = {}
    def __setitem__(self, key, val):
        if key in self.map and self.map[key] != val:
            raise AggregatorConflict(key, self.map[key], val)
        self.map[key] = val
    def __getitem__(self, key):
        try:
            return self.map[key]
        except KeyError:
            return None


# options
error_suppression = True
trace = None

agenda = prioritydict()
chart = chart_indirect()
errors = {}
changed = {}

# declarations
_agg_decl = aggregator_declaration()
_edges = defaultdict(set)
_updaters = defaultdict(list)
_initializers = []
_rules = {}


def dump_charts(out=sys.stdout):
    print >> out
    print >> out, 'Charts'
    print >> out, '============'

    fns = chart.keys()
    fns.sort()

    for x in fns:
        print >> out, chart[x]
        print >> out

    dump_errors(out)


def dump_errors(out=sys.stdout):
    if not errors:
        return
    # only print errors if we 'em.
    print >> out
    print >> out, 'Errors'
    print >> out, '============'
    for item, (val, es) in errors.items():
        print >> out,  'because %r is %r:' % (item, val)
        for e in es:
            print >> out, '   ', e
    print >> out


# TODO: codegen should output a derived Term instance for each functor
class Term(object):

    __slots__ = 'fn args value aggregator'.split()

    def __init__(self, fn, args):
        self.fn = fn
        self.args = args
        self.value = None
        self.aggregator = None

    def __cmp__(self, other):
        if other is None:
            return 1
        return cmp((self.fn, self.args), (other.fn, other.args))

    # default hash and eq suffice because we intern
    #def __hash__(self):
    #def __eq__(self):

    def __repr__(self):
        "Pretty print a term. Will retrieve the complete (ground) term."
        fn = '/'.join(self.fn.split('/')[:-1])  # drop arity from name.
        if not self.args:
            return fn
        return '%s(%s)' % (fn, ','.join(map(repr, self.args)))

    __add__ = __sub__ = __mul__ = notimplemented


def edges():
    def _emit(item, val, ruleix, variables):
        b = variables['nodes']
        b.sort()
        b = tuple(b)
        _edges[item].add((ruleix, b))
    for init in _initializers:
        init(emit=_emit)


class Chart(object):

    def __init__(self, name, arity):
        self.name = name
        self.arity = arity
        self.intern = {}   # args -> term
        self.ix = [defaultdict(set) for _ in xrange(arity)]

    def __repr__(self):
        rows = [term for term in self.intern.values() if term.value is not None]
        x = '\n'.join('%-30s := %r' % (term, term.value) for term in sorted(rows))
        return '%s\n=================\n%s' % (self.name, x)

    def __getitem__(self, s):
        assert len(s) == self.arity + 1, \
            'item width mismatch: arity %s, item %s' % (self.arity, len(s))

        args, val = s[:-1], s[-1]

        assert val is not None

        # filter set of candidates by each bound argument
        candidates = None
        for (ix, x) in zip(self.ix, args):
            if isinstance(x, slice):
                continue
            if candidates is None:
                # initial candidates determined by first non-bound column (if any)
                candidates = ix[x].copy()
            else:
                candidates &= ix[x]
                if not len(candidates):
                    # no candidates left
                    break

        if candidates is None:
            # This happens when all arguments are free.
            candidates = self.intern.values()

        # handle the value column separately because we don't index it yet.
        if isinstance(val, slice):
            for term in candidates:
                if term.value is not None:
                    yield term, term.args + (term.value,)
        else:
            for term in candidates:
                if term.value == val:
                    yield term, term.args + (term.value,)   # TODO: change codegen to avoid addition..

    def lookup(self, args):
        "find index for these args"
        assert len(args) == self.arity

        try:
            return self.intern[args]
        except KeyError:
            return None

    def insert(self, args, val):

        # debugging check: row is not already in chart.
        assert self.lookup(args) is None, \
            '%r already in chart with value %r' % (args, val)

        self.intern[args] = term = Term(self.name, args)
        term.value = val
        term.aggregator = aggregator(_agg_decl[self.name])

        # indexes new term
        for i, x in enumerate(args):
            self.ix[i][x].add(term)

        return term


def build(fn, *args):
    # TODO: codegen should handle true/0 is True and false/0 is False
    if fn == "true/0":
        return True
    if fn == "false/0":
        return False
    term = chart[fn].lookup(args)
    if term is None:
        term = chart[fn].insert(args, None)   # don't know val yet.
    return term


# Update handler indirection -- a temporary hack. Allow us to have many handlers
# on the same functor/arity. Eventually, we'll fuse handlers into one handler.

def register(fn):
    """
    Decorator for registering update handlers. Used by update dispatcher.

    Note: registration is with a global/mutable table.
    """

    def wrap(handler):
        handler.dyna_attrs = parse_attrs(handler)
        _updaters[fn].append(handler)
        # you can't call these guys directly. Must go thru handler
        # indirection table
        return None

    return wrap


# - "initializers" aren't just initializers -- They are fully-naive bottom-up
#   inference routines. At the moment we only use them to initialize the chart.

from utils import parse_attrs

def initializer(_):
    "Implementation idea is very similar to register."

    def wrap(handler):
        handler.dyna_attrs = parse_attrs(handler)
        _initializers.append(handler)
        return None

    return wrap


def update_dispatcher(item, val, delete):
    """
    Passes update to relevant handlers.
    """

    if val is None:
        return

    for handler in _updaters[item.fn]:

        emittiers = []
        _emit = lambda item, val, ruleix, variables: \
            emittiers.append((item, val, ruleix, variables, delete))

        try:
            handler(item, val, emit=_emit)
        except (TypeError, ZeroDivisionError) as e:
            if error_suppression:
                #print >> trace,
                print '%s on update %s = %s' % (e, item, val)

                if item not in errors:
                    errors[item] = (val, [])

                errors[item][1].append('%s\n        in rule %s\n            %s' % \
                                           (e,
                                            handler.dyna_attrs['Span'],
                                            handler.dyna_attrs['rule']))

            else:
                raise e
        else:
            # no exception, accept emissions.
            for e in emittiers:
                # an error could happen here, but we assume (by contract) that
                # this is not possible.
                emit(*e)


def emit(item, val, ruleix, variables, delete):

    print >> trace, (red % 'delete' if delete else green % 'update'), \
        '%s (val %s; curr: %s)' % (item, val, item.value)

    if delete:
        item.aggregator.dec(val, ruleix, variables)
    else:
        item.aggregator.inc(val, ruleix, variables)

    agenda[item] = 0   # everything is high priority


def peel(fn, item):
    """
    Find item's args in the appropriate chart. Assert that idx matches
    functor/arity, `fn`. Returns the arguments of term as a tuple of intern idxs
    and constants (possibly an empty tuple).
    """

    if fn == "true/0" :
        assert item is True
        return
    if fn == "false/0" :
        assert item is False
        return
    assert isinstance(item, Term)
    assert item.fn == fn
    return item.args


def _go():
    "the main loop"

    changed.clear()

    while agenda:
        item = agenda.pop_smallest()

        print >> trace
        print >> trace, magenta % 'pop   ', item,

        was = item.value
        print >> trace, '(was: %s,' % (was,),

        try:
            now = item.aggregator.fold()
        except (ZeroDivisionError, TypeError) as e:
            errors[item] = ('failed to aggregate %r' % item.aggregator, [e])
            # TODO: Are we sure there is never a reason to requeue this item.
            continue

        print >> trace, 'now: %s)' % (now,)

        if was == now:
            print >> trace, yellow % 'unchanged'
            continue

        was_error = False
        if item in errors:    # clear the error
            was_error = True
            del errors[item]

        # TODO: handle `was` and `now` at the same time to avoid the two passes.
        if was is not None and not was_error:

            # if `was` resulted in an error we know it didn't propagate so we
            # can skip running the update dispatcher in delete mode.

            update_dispatcher(item, was, delete=True)

        item.value = now

        if now is not None:
            update_dispatcher(item, now, delete=False)

        changed[item] = now


def go():
    try:
        _go()
    except KeyboardInterrupt:
        pass


def dynac(f, out):
    return os.system('%s/dist/build/dyna/dyna -B python -o "%s" "%s"' % (dynahome, out, f))


def dynac_code(code, debug=False, run=True):
    "skip the file."

    dyna = '/tmp/tmp.dyna'

    out = '%s.plan.py' % dyna

    with file(dyna, 'wb') as f:
        f.write(globals().get('parser_state', ''))  # include parser state if any.
        f.write(code)

    if dynac(dyna, out):   # stop if the compiler failed.
        return True

    if debug:
        import debug
        debug.main(dyna)

    if run:
        do(out)


def load(f):

    with file(f) as h:
        print >> trace, magenta % 'Loading new code'
        print >> trace, yellow % h.read()

    # TODO: loading new code should be atomic. if we fail for some reason we
    # need to revert.

    # load generated code.
    execfile(f, globals())     # if we want to isolate side-effects of new code
                               # we can pass in something insead of globals()


def dump(code, filename='/tmp/tmp.dyna'):
    "Write code to file."
    with file(filename, 'wb') as f:
        f.write(code)
    return filename


def do(filename):
    "Compile, load, and execute dyna code."

    assert os.path.exists(filename)

    global _initializers
    _initializers = []     # XXX: do we really want to clear?

    load(filename)

    for init in _initializers:   # assumes we have cleared

        def _emit(head, val, *args, **kw):
            return emit(head, val, *args, delete=False, **kw)

        try:
            init(emit=_emit)
        except (TypeError, ZeroDivisionError) as e:
            raise DynaInitializerException(e, init)

    go()


import cmd, readline

class REPL(cmd.Cmd, object):

    def __init__(self, hist):
        cmd.Cmd.__init__(self)
        self.hist = hist
        if not os.path.exists(hist):
            readline.clear_history()
            with file(hist, 'wb') as f:
                f.write('')
        readline.read_history_file(hist)
        self.do_trace('off')
        self.lineno = 0

    @property
    def prompt(self):
        return ':- ' #% self.lineno

    def do_exit(self, _):
        readline.write_history_file(self.hist)
        return -1

    def do_EOF(self, args):
        "Exit on end of file character ^D."
        print 'exit'
        return self.do_exit(args)

    def precmd(self, line):
        """
        This method is called after the line has been input but before it has
        been interpreted. If you want to modify the input line before execution
        (for example, variable substitution) do it here.
        """
        return line

    def postcmd(self, stop,  line):
        self.lineno += 1
        return stop

    def do_changed(self, _):
        if not changed:
            return
        print '============='
        for x, v in sorted(changed.items()):
            print '%s := %r' % (x, v)
        print
        dump_errors()

    def do_chart(self, args):
        if not args:
            dump_charts()
        else:
            unrecognized = set(args.split()) - set(chart.keys())
            for f in unrecognized:
                print 'unrecognized predicate', f
            if unrecognized:
                print 'available:\n\t' + '\t'.join(chart.keys())
                return
            for f in args.split():
                print chart[f]
                print
            dump_errors()

    def emptyline(self):
        """Do nothing on empty input line"""
        pass

    def do_ip(self, _):
        ip()

    def do_go(self, _):
        go()

    def do_trace(self, args):
        global trace
        if args == 'on':
            trace = sys.stdout
        elif args == 'off':
            trace = file(os.devnull, 'w')
        else:
            print 'Did not understand argument %r please use (on or off).' % args

    def do_debug(self, line):
        dynac_code(line, debug=True, run=False)

    def do_query(self, line):

        if line.endswith('.'):
            print "Queries don't end with a dot."
            return

        query = 'out(%s) dict= _VALUE is (%s), _VALUE.' % (self.lineno, line)

        print blue % query

        self.default(query)

        for (_, results) in chart['out/1'][self.lineno,:]:
            for result in results:
                print result
        print

    def default(self, line):
        """
        Called on an input line when the command prefix is not recognized.  In
        that case we execute the line as Python code.
        """
        line = line.strip()
        if not line.endswith('.'):
            print "ERROR: Line doesn't end with period."
            return
        try:
            if dynac_code(line):  # failure.
                return
        except AggregatorConflict as e:
            print 'AggregatorConflict:', e
        else:
            self.do_changed('')

    def do_draw(self, _):
        draw()

    def cmdloop(self, _=None):
        try:
            super(REPL, self).cmdloop()
        except KeyboardInterrupt:
            print '^C'
            self.cmdloop()
        except Exception as e:
            readline.write_history_file(self.hist)
            raise e


def repl(hist):
    REPL(hist).cmdloop()


def hypergraph():
    from debug import Hypergraph
    # collect edges
    edges()
    # create hypergraph object
    g = Hypergraph()
    for c in chart.values():
        for x in c.intern.values():
            for e in _edges[x]:
                label, body = e
                g.edge(str(x), str(label), map(str, body))
    return g


def draw():
    g = hypergraph()
    with file('/tmp/state.html', 'wb') as f:
        print >> f, """
        <html>
        <head>
        <style>
        body {
          background-color: black;
          color: white;
        }
        </style>
        </head>
        <body>
        """

        x = StringIO()
        dump_charts(x)

        print >> f, '<div style="position:absolute;">%s</div>' \
            % '<h1>Charts</h1>%s' \
            % '<pre style="width: 500px;">%s</pre>' \
            % x.getvalue()

        print >> f, """
<div style="width: 800px; position:absolute; left: 550px">
<h1>Hypergraph</h1>
%s
</div>
""" % g.render('/tmp/hypergraph')

        print >> f, '</body></html>'

    webbrowser.open(f.name)

def main():
#    from repl import repl

    parser = ArgumentParser(description="The dyna interpreter!")
    parser.add_argument('source', help='Path to Dyna source file (or plan if --plan=true).', nargs='?')
    parser.add_argument('--plan', action='store_true', default=False,
                        help='`source` specifies output of the compiler instead of dyna source code.')
    parser.add_argument('--trace', default='/tmp/dyna.log')
    parser.add_argument('-i', dest='interactive', action='store_true', help='Fire-up an IPython shell.')
    parser.add_argument('-o', dest='output', help='Output chart.')
    parser.add_argument('--draw', action='store_true',
                        help='Output html page with hypergraph and chart.')

    argv = parser.parse_args()

    global trace
    if argv.trace == 'stderr':
        trace = sys.stderr
    elif argv.trace == 'stdout':
        trace = sys.stdout
    else:
        trace = file(argv.trace, 'wb')

    if argv.source:

        if not os.path.exists(argv.source):
            print 'File %r does not exist.' % argv.source
            return

        if argv.plan:
            plan = argv.source
        else:
            plan = "%s.plan.py" % argv.source
            dynac(argv.source, plan)

        do(plan)

        if argv.output:
            if argv.output == "-":
                dump_charts(sys.stdout)
            else:
                with file(argv.output, 'wb') as f:
                    dump_charts(f)
        else:
            dump_charts()

        if argv.interactive:
            repl(hist = argv.source + '.hist')

    else:
        repl(hist = '/tmp/dyna.hist')

    if argv.draw:
        draw()



if __name__ == '__main__':
    main()
