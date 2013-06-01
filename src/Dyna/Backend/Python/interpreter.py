#!/usr/bin/env python
from __future__ import division

"""


===

This has an absurd parse:
  x += f('result = 5').

set= is wrong .. needs to keep counts like bag=

===


Warnings/lint checking
======================

 - Catch typos! Warn the user if they write a predicate that is not defined on
   the LHS of a rule and it's not quoted (i.e. not some new piece of structure).


===

 - "initializers" aren't just initializers, they are the fully-naive bottom-up
   inference rules.

 - TODO: routines from probing the chart.

 - XXX: maybe the chart should store pretty printed term and a reference to the
   aggregator (each item get's its own aggregator to avoid a hash lookup).

 - XXX: should we store value and aggregators separate from others columns? that
   is, separate the chart and intern table.

 - TODO: should probably make a Term object.

 - XXX: we should probably fuse update handlers instead of dispatching to each
   one independently.

 - TODO: deleting a rule: (1) remove update handlers (2) run initializers in
   delete mode (3) remove initializers.

 - TODO: hooks from introspection, eval, and prioritization.

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

"""

#from debug import ultraTB2; ultraTB2.enable()
#from debug import saverr; saverr.enable(editor=True)

import os, sys
from cStringIO import StringIO
from collections import defaultdict
from argparse import ArgumentParser
from utils import ip, red, green, blue, magenta, yellow, dynahome
from defn import agg_bind


class AggregatorConflict(Exception):
    pass


#def eval(x):
#    exec x in globals(), locals()
#    if 'result' in locals():
#        return locals()['result']


trace = None

# TODO: as soon as we have safe names for these things we can get rid of this.
class chart_indirect(dict):
    def __missing__(self, key):
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(name = key, ncols = arity + 1)  # +1 for value
        return c


class aggregator_indirect(dict):
    def __missing__(self, item):
        a = agg_bind(item, agg_decl)
        self[item] = a
        return a


aggregator = aggregator_indirect()


chart = chart_indirect()

_delete = False
agenda = set()
agg = {}

# when a new rule comes along it puts a string in the following dictionary
class aggregator_declaration(object):
    def __init__(self):
        self.map = {}
    def __setitem__(self, key, value):
        if key in self.map and self.map[key] != value:
            raise AggregatorConflict("Aggregator conflict %s was %r trying to set to %r." % (key, self.map[key], value))
        self.map[key] = value
    def __getitem__(self, key):
        return self.map[key]

agg_decl = aggregator_declaration()


def dump_charts(out=sys.stdout):
    print >> out
    print >> out, 'Charts'
    print >> out, '============'

    fns = chart.keys()
    fns.sort()

    for x in fns:
        print >> out, chart[x]
        print >> out


from collections import namedtuple
class Term(namedtuple('Term', 'fn idx')):

    def __init__(self, fn, idx):
        self.row = chart[fn].data[idx]           # mutable reference to row in chart
        super(Term, self).__init__(fn, idx)

    @property
    def args(self):
        return self.row[:-1]

    @property
    def value(self):
        return self.row[-1]

    @value.setter
    def value(self, now):
        self.row[-1] = now

    def __repr__(self):
        return pretty(self)


def pretty(item):
    "Pretty print a term. Will retrieve the complete (ground) term from the chart."
    if not isinstance(item, tuple):
        return repr(item)
    row = item.row
    args = row[:-1]
    fn = ''.join(item.fn.split('/')[:-1])  # drop arity from name.
    pretty_args = map(pretty, args)
    if not len(pretty_args):          # zero arity -> no parens.
        return fn
    return '%s(%s)' % (fn, ','.join(pretty_args))


class Chart(object):

    def __init__(self, name, ncols):
        self.name = name
        self.ncols = ncols
        self.data = {}
        self._id = 0

    def __repr__(self):
        rows = [(r, Term(self.name, i), r[-1]) for i, r in self.data.items() if r[-1] is not None]
        x = '\n'.join('%-30s := %s' % (p, v) for _, p, v in sorted(rows))
        return '%s\n=================\n%s' % (self.name, x)

    def __getitem__(self, item):
        assert isinstance(item, tuple) and len(item) == self.ncols, \
            'item width mismatch: ncols %s, item %s' % (self.ncols, len(item))
        nonslice = [(i, val) for i, val in enumerate(item) if not isinstance(val, slice)]
        slices = [i for i, val in enumerate(item) if isinstance(val, slice)]

        # XXX: very inefficient
        for row in self.data.values():  # note: dict changes size during iteration
            if row[-1] is None:
                continue
            if all(row[i] == val for i, val in nonslice):
                yield tuple(row[i] for i in slices)

    def lookup(self, args):
        "find index for these args"
        assert len(args) == self.ncols - 1                    # XXX: lookup doesn't want val?
        args = list(args)
        for idx, row in self.data.iteritems():                # XXX: very inefficient
            if row[:-1] == args:
                return idx             # stop on first

    def update(self, ix, args):
        "Update chart"
        assert len(args) == self.ncols and isinstance(args, list)
        self.data[ix] = args

    def insert(self, args, val):

        args = list(args)
        args.append(val)

        # debugging check: row is not already in chart.
        assert self.lookup(args[:-1]) is None, '%s already in chart' % (args,)

        idx = self.next_id()
        self.update(idx, args)
        return idx

    def next_id(self):
        idx = self._id
        self._id += 1
        return idx


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


def initializer(_):
    "Same idea as register"

    def wrap(handler):
        initializer.handlers.append(handler)
        return None

    return wrap

initializer.handlers = []


def update_dispatcher(item, val):
    """
    Passes update to relevant handlers.
    """
    if val is None:
        return
    for handler in register.handlers[item.fn]:
        handler(item, val)


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


def build(fn, *args):
    if fn == "true/0":
        return True
    if fn == "false/0":
        return False
    idx = chart[fn].lookup(args)
    if idx is None:
        idx = chart[fn].insert(args, None)   # don't know val yet.
    return Term(fn, idx)


def emit(item, val, ruleix=None, variables=None):

    print >> trace, (red % 'delete' if _delete else green % 'update'), \
        '%s (val %s; curr: %s)' % (pretty(item), val, item.value)

    if _delete:
        aggregator[item].dec(val)
    else:
        aggregator[item].inc(val)

    agenda.add(item)


def delete(item, val):
    # XXX: very ugly handling of deletion by global variable; should probably
    # target only handler at a time, because this will get called more times
    # than it should.
    global _delete
    _delete = True
    update_dispatcher(item, val)
    _delete = False


changed = {}

def _go():
    "the main loop"

    changed.clear()

    while agenda:
        item = agenda.pop()

        print >> trace
        print >> trace, magenta % 'pop   ', pretty(item),

        was = item.value
        print >> trace, '(was: %s,' % was,

        now = aggregator[item].fold()
        print >> trace, 'now:', str(now) + ')'

        if was == now:
            print >> trace, yellow % 'unchanged'
            continue

        if was is not None:
            delete(item, was)

        item.value = now

        if now is not None:
            update_dispatcher(item, now)

        changed[item] = now


def go():
    try:
        _go()
    except KeyboardInterrupt:
        pass


def dynac(f, out):
    cmd = '%s/dist/build/dyna/dyna -B python -o "%s" "%s"' % (dynahome, out, f)
    if os.system(cmd):
        print 'command failed:\n\t' + cmd
        return True


def dynac_code(code, debug=0):
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

    with file(out) as f:
        new_code = f.read()

    do(out)


def load(f):

    with file(f) as h:
        print >> trace, magenta % 'Loading new code'
        print >> trace, yellow % h.read()

    # load generated code.
    execfile(f, globals())


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
        init()

    go()


import cmd

class REPL(cmd.Cmd, object):
    """
    ## console.py
    ## Author:   James Thiele
    ## Date:     27 April 2004
    ## Version:  1.0
    ## Location: http://www.eskimo.com/~jet/python/examples/cmd/
    ## Copyright (c) 2004, James Thiele
    """
    def __init__(self):
        cmd.Cmd.__init__(self)
        self.prompt = ":- "
        #self.intro  = "Welcome to console!"

    ## Command definitions ##
#    def do_hist(self, args):
#        """Print a list of commands that have been entered"""
#        print self._hist

    def do_exit(self, _):
        """Exits from the console"""
        return -1

    ## Command definitions to support Cmd object functionality ##
    def do_EOF(self, args):
        """Exit on system end of file character"""
        return self.do_exit(args)

    def do_help(self, args):
        """Get help on commands
           'help' or '?' with no arguments prints a list of commands for which help is available
           'help <command>' or '? <command>' gives help on <command>
        """
        ## The only reason to define this method is for the help text in the doc string
        cmd.Cmd.do_help(self, args)

    ## Override methods in Cmd object ##
    def preloop(self):
        """Initialization before prompting user for commands.
           Despite the claims in the Cmd documentaion, Cmd.preloop() is not a stub.
        """
        cmd.Cmd.preloop(self)   ## sets up command completion
        self._hist    = []      ## No history yet
        self._locals  = {}      ## Initialize execution namespace for user
        self._globals = {}

    def postloop(self):
        """Take care of any unfinished business.
           Despite the claims in the Cmd documentaion, Cmd.postloop() is not a stub.
        """
        cmd.Cmd.postloop(self)   ## Clean up command completion
        print "Exiting..."

    def precmd(self, line):
        """
        This method is called after the line has been input but before it has
        been interpreted. If you want to modifdy the input line before execution
        (for example, variable substitution) do it here.
        """
        self._hist += [ line.strip() ]
        return line

    def do_changed(self, _):
        if not changed:
            print 'nothing changed.'
            print
            return

        print
        print 'Changed'
        print '============='
        for x, v in changed.items():
            print pretty(x), ':=', v
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

    def postcmd(self, stop, line):
        """
        If you want to stop the console, return something that evaluates to true.
        If you want to do some post command processing, do it here.
        """
        return stop

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

    def do_query(self, line):

        if line.endswith('.'):
            print "Queries don't end with a dot."
            return

        query = 'zzz set= _VALUE is %s, eval("print 12345"), _VALUE.' % line

        print blue % query

        self.default(query)


    def default(self, line):
        """Called on an input line when the command prefix is not recognized.
           In that case we execute the line as Python code.
        """

        line = line.strip()

        if not line.strip().endswith('.'):
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


def repl():
    REPL().cmdloop()



def main():
    parser = ArgumentParser(description="The dyna interpreter!")
    parser.add_argument('source', help='Path to Dyna source file (or plan if --plan=true).')
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
        repl()


if __name__ == '__main__':
    main()
