#!/usr/bin/env python

"""

MISC
====

 - TODO: create an Interpreter object to hold state.

 - TODO: deleting a rule: (1) remove update handlers (2) run initializers in
   delete mode (3) remove initializers.

 - TODO: hooks from introspection, eval, and prioritization.

     whats the default prioritization?

 - TODO: Term's should only be aggregated with ``=`` or ``:=``. We should
   disallow ``a += &b.``

     Equals aggregation only one value allowed, mult. >0 on single value. The
     following program has one FP of `c` end `e` are mutually exclusive.

       a = b for c
       a = d for e

     This might not be the case during computation -- this is the same as the
     error problem.

 - TODO: doc tests for Dyna code!

 - TODO: repl needs to pass parser a rule index pragma to start from.

      blocked: nwf will tell me what bits of parser state to send back to him.

 - TODO: build hypergraph from unrolled circuit. This requires a little bit of
   thinking because we don't yet know what things in the chart have been
   touched.

 - TODO: transactions for errors.



PARSER
======

  - TODO: Nested expressions:

    out(0) dict= _VALUE is (rewrite(X,Y) + rewrite(X,Y,Z)), _VALUE.

    FATAL: Encountered error in input program:
     Parser error
     /tmp/tmp.dyna:1:14: error: expected: "."
     rewrite(X,Y) + rewrite(X,Y,Z)<EOF>
                  ^


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

   The good: we throw and AggregatorConflict exception if newly loaded code
   tries to overwrite an aggregator in `agg_decl`.

   However, we need to make sure that we don't load subsequent code pertaining
   to this rule that we should reject altogether.

   timv: At the moment I believe we're safe because agg_decl is set before any
   of the registers (i.e. it's at the top of the generated code). This obviously
   isn't the best way to do this, but we're going to have to overhaul this
   entire infrastructure soon to handle rule-retraction.. So we can fix this
   later.


INTERPRETER
===========

 - Error values (with provenance ideally)

   Consider the following program:
    | c += 0
    | b += 1
    | a += b / c
    | d += a
    | e += d

   Results in the fixed point:
    | c = 0
    | b = 1
    | a = error("divison by zero in rule 'a += b / c'")
    | d = error("because a = error in 'rule d += a.'")   # because error annihilate aggregators
    | e = error("because of d ...")

  Should errors have linear provenance? The error could have come from more than
  one parent.


  The reason we have to support error is because the system might need to go
  through an error state before it can reach it's fixed out. In order to be
  invariant to execution order (preserve our semantice) we to need to have the
  ability fo reach pass thru an error state.

  for example:
   :- a += 1/c.
   :- b += 0.         % (1)
   :- b += 1.         % (2)

  If we process (1) before (2) we get an error value for `a` due to the divide
  by zero but then once (2) is processed the error should go away because `b` is
  no longer `0`. Whereas, (2) before (1) is ok!

  timv: This isn't sufficiently motivating because we can just leave `a` as
  `null` until we pass the divide by zero error.




"""

from __future__ import division
import os, sys
from collections import defaultdict, namedtuple
from functools import partial
from argparse import ArgumentParser

from utils import ip, red, green, blue, magenta, yellow, dynahome, \
    notimplemented, prioritydict
from defn import aggregator


class AggregatorConflict(Exception):
    pass


# TODO: as soon as we have safe names for these things we can get rid of this.
class chart_indirect(dict):
    def __missing__(self, key):
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(name = key, ncols = arity + 1)  # +1 for value
        return c


# when a new rule comes along it puts a string in the following dictionary
class aggregator_declaration(object):
    def __init__(self):
        self.map = {}
    def __setitem__(self, key, value):
        if key in self.map and self.map[key] != value:
            raise AggregatorConflict("Aggregator conflict %s was %r trying to "
                                     "set to %r." % (key, self.map[key], value))
        self.map[key] = value
    def __getitem__(self, key):
        try:
            return self.map[key]
        except KeyError:
            return None


error_suppression = False
trace = None
agenda = prioritydict()
agg_decl = aggregator_declaration()
chart = chart_indirect()


def dump_charts(out=sys.stdout):
    print >> out
    print >> out, 'Charts'
    print >> out, '============'

    fns = chart.keys()
    fns.sort()

    for x in fns:
        print >> out, chart[x]
        print >> out


# TODO: codegen should output a derived Term instance for each functor
class Term(namedtuple('Term', 'fn args'), object):

    def __init__(self, fn, args):
        self._value = None
        self.aggregator = None
        super(Term, self).__init__(fn, args)

    def __repr__(self):
        "Pretty print a term. Will retrieve the complete (ground) term."
        fn = '/'.join(self.fn.split('/')[:-1])  # drop arity from name.
        if not self.args:
            return fn
        return '%s(%s)' % (fn, ','.join(map(repr, self.args)))

    __add__ \
        = __sub__ \
        = __mul__ \
        = notimplemented

#    @property
#    def value(self):
#        return self._value

#    @value.setter
#    def value(self, val):
#        assert not isinstance(val, tuple) or isinstance(val, Term)
#        self._value = val


class Chart(object):

    def __init__(self, name, ncols):
        self.name = name
        self.ncols = ncols
        self.intern = {}   # args -> term
        self.ix = [defaultdict(set) for _ in xrange(ncols-1)]  # TODO: no index on values yet.

    def __repr__(self):
        rows = [term for term in self.intern.values() if term.value is not None]
        x = '\n'.join('%-30s := %r' % (term, term.value) for term in sorted(rows))
        return '%s\n=================\n%s' % (self.name, x)

    def __getitem__(self, s):

        assert isinstance(s, tuple) and len(s) == self.ncols, \
            'item width mismatch: ncols %s, item %s' % (self.ncols, len(s))

        args, val = s[:-1], s[-1]

        assert val is not None

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
                    break

        # all arguments must be bound.
        if candidates is None:
            candidates = self.intern.values()

        # handle the value column separately because we don't index it yet.
        if isinstance(val, slice):
            for term in candidates:
                if term.value is not None:
                    yield term.args + (term.value,)
        else:
            for term in candidates:
                if term.value == val:
                    yield term.args + (term.value,)

    def lookup(self, args):
        "find index for these args"
        assert len(args) == self.ncols - 1

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
        term.aggregator = aggregator(agg_decl[self.name])

        # indexes new term
        for i, x in enumerate(args):
            self.ix[i][x].add(term)

        return term


def build(fn, *args):
    if fn == "true/0":   # TODO: I'd rather have the codegen ensure true/0 is True and false/0 is False
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

    For testing purposes clear current handlers
    >>> register.handlers.clear()

    >>> @register("test")
    ... def _(H, V):
    ...     return 'OK', H, V
    ...
    >>> [h] = register.handlers['test']
    >>> h("head", "val")
    ('OK', 'head', 'val')

    """

    def wrap(handler):
        register.handlers[fn].append(handler)
        # you can't call these guys directly. Must go thru handler
        # indirection table
        return None

    return wrap

register.handlers = defaultdict(list)


# - "initializers" aren't just initializers -- They are fully-naive bottom-up
#   inference routines. At the moment we only use them to initialize the chart.

def initializer(_):
    "Implementation idea is very similar to register."

    def wrap(handler):
        initializer.handlers.append(handler)
        return None

    return wrap

initializer.handlers = []


def update_dispatcher(item, val, delete):
    """
    Passes update to relevant handlers.
    """
    if val is None:
        return
    for handler in register.handlers[item.fn]:

        emittiers = []
        _emit = lambda item, val, ruleix, variables: \
            emittiers.append(lambda: emit(item, val, ruleix, variables, delete=delete))

        try:
            handler(item, val, emit=_emit)
        except (TypeError, ZeroDivisionError) as e:
            if error_suppression:
                #print >> trace,
                print '%s on update %s = %s' % (e, item, val)
            else:
                raise e
        else:
            # no exception, accept emissions.
            for e in emittiers:
                e()


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


def emit(item, val, ruleix, variables, delete):

    print >> trace, (red % 'delete' if delete else green % 'update'), \
        '%s (val %s; curr: %s)' % (item, val, item.value)

    if delete:
        item.aggregator.dec(val, ruleix, variables)
    else:
        item.aggregator.inc(val, ruleix, variables)

    agenda[item] = 0   # everything is high priority


changed = {}

def _go():
    "the main loop"

    changed.clear()

    while agenda:
        item = agenda.pop_smallest()

        print >> trace
        print >> trace, magenta % 'pop   ', item,

        was = item.value
        print >> trace, '(was: %s,' % (was,),

        now = item.aggregator.fold()
        print >> trace, 'now: %s)' % (now,)

        if was == now:
            print >> trace, yellow % 'unchanged'
            continue

        # TODO: handle was and now at the same time to avoid the two passes.
        if was is not None:
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

    initializer.handlers = []    # XXX: do we really want to clear?

    load(filename)

    for init in initializer.handlers:   # assumes we have cleared

        _emit = partial(emit, delete=False)

        try:
            init(emit=_emit)
        except (TypeError, ZeroDivisionError) as e:
            if error_suppression:
                #print >> trace,
                print e, 'in initializer.'
            else:
                raise e

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

def main():
#    from repl import repl

    parser = ArgumentParser(description="The dyna interpreter!")
    parser.add_argument('source', help='Path to Dyna source file (or plan if --plan=true).', nargs='?')
    parser.add_argument('--plan', action='store_true', default=False,
                        help='`source` specifies output of the compiler instead of dyna source code.')
    parser.add_argument('--trace', default='/tmp/dyna.log')
    parser.add_argument('-i', dest='interactive', action='store_true', help='Fire-up an IPython shell.')
    parser.add_argument('-o', dest='output', help='Output chart.')

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


if __name__ == '__main__':
    main()
