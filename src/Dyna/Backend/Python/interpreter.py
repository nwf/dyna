#!/usr/bin/env python

"""
MISC
====

 - TODO: faster charts (dynamic argument types? jason's trie data structure)

 - TODO: write files to ~/.dyna

 - TODO: (@nwf) String quoting (see example/stringquote.py)

 - TODO: filter and bulk loader

 - TODO: make sure interpreter uses the right exceptions. The codegen catches a
   few things -- I think assertionerror is one them... we should probably do
   whatever this is doing with a custom exception.

 - TODO: (@nwf) mode planning failures are slient

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

 - TODO: Numeric precision is an issue with BAggregators.

     timv: Are we sure we have this bug?

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

"""

from __future__ import division
import os, sys
from collections import defaultdict
from argparse import ArgumentParser

import debug
from chart import Chart, Term
from defn import aggregator
from utils import ip, red, green, blue, magenta, yellow, dynahome, \
    notimplemented, prioritydict, parse_attrs
from config import dotdynadir


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


# TODO:
class DynaCompilerError(Exception):
    pass


class Interpreter(object):

    def __init__(self):
        # declarations
        self.agg_name = {}
        self.edges = defaultdict(set)
        self.updaters = defaultdict(list)
        self.initializers = []
        # data structures
        self.agenda = prioritydict()
        self.parser_state = ''

        agg = self.agg_name
        class dd(dict):
            def __missing__(self, fn):
                arity = int(fn.split('/')[-1])
                self[fn] = c = Chart(fn, arity, lambda: aggregator(agg[fn]))
                return c
        self.chart = dd()

        self.errors = {}
        # misc
        self.trace = file(dotdynadir / 'trace', 'wb')

    def new_fn(self, fn, agg):
        if fn in self.agg_name:
            # check for aggregator conflict.
            if self.agg_name[fn] != agg:
                raise AggregatorConflict(fn, self.agg_name[fn], agg)
            return
        self.agg_name[fn] = agg

    def collect_edges(self):
        """
        Use rule initializers to find all active hyperedges in the current
        Chart.
        """
        edges = self.edges
        def _emit(item, _, ruleix, variables):
            b = variables['nodes']
            b.sort()
            b = tuple(b)
            edges[item].add((ruleix, b))
        for init in self.initializers:
            init(emit=_emit)

    def dump_charts(self, out=sys.stdout):
        print >> out
        print >> out, 'Charts'
        print >> out, '============'
        fns = self.chart.keys()
        fns.sort()
        for x in fns:
            print >> out, self.chart[x]
            print >> out
        self.dump_errors(out)

    def dump_errors(self, out=sys.stdout):
        # We only dump the error chart if it's non empty.
        if not self.errors:
            return
        print >> out
        print >> out, 'Errors'
        print >> out, '============'
        for item, (val, es) in self.errors.items():
            print >> out,  'because %r is %r:' % (item, val)
            for e in es:
                print >> out, '   ', e
        print >> out

    def build(self, fn, *args):
        # TODO: codegen should handle true/0 is True and false/0 is False
        if fn == "true/0":
            return True
        if fn == "false/0":
            return False

        # FIXME:
        if fn not in self.agg_name:
            # TODO: if the item has no aggregator (e.g purely structural stuff)
            self.new_fn(fn, None)

        term = self.chart[fn].lookup(args)
        if term is None:
            term = self.chart[fn].insert(args, None)   # don't know val yet.
        return term

    def go(self):
        "the main loop"

        changed = {}
        agenda = self.agenda
        errors = self.errors
        trace = self.trace

        while agenda:
            item = agenda.pop_smallest()

            print >> trace
            print >> trace, magenta % 'pop   ', item,

            was = item.value
            print >> trace, '(was: %r,' % (was,),

            try:
                now = item.aggregator.fold()
            except (ZeroDivisionError, TypeError) as e:
                errors[item] = ('failed to aggregate %r' % item.aggregator, [e])
                # TODO: Are we sure there is never a reason to requeue this item.
                continue

            print >> trace, 'now: %r)' % (now,)

            if was == now:
                print >> trace, yellow % 'unchanged'
                continue

            was_error = False
            if item in errors:    # clear the error
                was_error = True
                del errors[item]

            # TODO: handle `was` and `now` at the same time to avoid the two passes.
            # TODO: will need to propagate was=None when we have question mark
            if was is not None and not was_error:
                # if `was` is marked as an error we know it didn't propagate.
                # Thus, we can skip the delete-updates.
                self.update_dispatcher(item, was, delete=True)

            item.value = now

            if now is not None:
                self.update_dispatcher(item, now, delete=False)

            changed[item] = now

        return changed

    def update_dispatcher(self, item, val, delete):
        """
        Passes update to relevant handlers.
        """

        # XXX: fix for `?` prefix operator
        assert val is not None

        # store emissions, make sure all of them succeed before propagating
        # changes to aggregators.
        emittiers = []
        t_emit = lambda item, val, ruleix, variables: \
            emittiers.append((item, val, ruleix, variables, delete))

        try:

            # TODO: do we want to collect all handlers with errors?
            for handler in self.updaters[item.fn]:
                handler(item, val, emit=t_emit)

        except (TypeError, ZeroDivisionError) as e:

            if item not in self.errors:
                self.errors[item] = (val, [])

            # TODO: don't eagerly format the message.
            self.errors[item][1].append('%s\n        in rule %s\n            %s' % \
                                            (e,
                                             handler.dyna_attrs['Span'],
                                             handler.dyna_attrs['rule']))

        else:
            # no exception, accept emissions.
            for e in emittiers:
                # an error could happen here, but we assume (by contract) that
                # this is not possible.
                self.emit(*e)

    def new_updater(self, fn, handler):
        self.updaters[fn].append(handler)

    def new_initializer(self, init):
        self.initializers.append(init)

    def emit(self, item, val, ruleix, variables, delete):
        print >> self.trace, (red % 'delete' if delete else green % 'update'), \
            '%s (val %s; curr: %s)' % (item, val, item.value)

        if delete:
            item.aggregator.dec(val, ruleix, variables)
        else:
            item.aggregator.inc(val, ruleix, variables)

        self.agenda[item] = 0   # everything is high priority

    def repl(self, hist):
        import repl
        repl.REPL(self, hist).cmdloop()

    def do(self, filename):
        "Compile, load, and execute dyna code."

        assert os.path.exists(filename)

        # for debuggging
        with file(filename) as h:
            print >> self.trace, magenta % 'Loading new code'
            print >> self.trace, yellow % h.read()

        # TODO: loading new code should be atomic. if we fail for some reason we
        # need to revert.

        env = {'_initializers': [], '_updaters': [], '_agg_decl': {},
               'chart': self.chart, 'build': self.build, 'peel': peel,
               'parser_state': None}

        # load generated code.
        execfile(filename, env)

        emits = []
        def _emit(*args):
            emits.append(args)

        for k, v in env['_agg_decl'].items():
            self.new_fn(k, v)
        for fn, h in env['_updaters']:
            h.dyna_attrs = parse_attrs(h)
        for h in env['_initializers']:
            h.dyna_attrs = parse_attrs(h)

        try:
            # only run new initializers
            for init in env['_initializers']:
                init(emit=_emit)

        except (TypeError, ZeroDivisionError) as e:
            raise DynaInitializerException(e, init)

        else:
            # add new updaters
            for fn, h in env['_updaters']:
                self.new_updater(fn, h)

            # add new initializers
            for h in env['_initializers']:
                self.new_initializer(h)

            # process emits
            for e in emits:
                self.emit(*e, delete=False)

            self.parser_state = env['parser_state']

        return self.go()

    def draw(self):
        debug.draw(self)

    def dynac_code(self, code):
        dyna = dotdynadir / 'tmp.dyna'
        out = '%s.plan.py' % dyna

        with file(dyna, 'wb') as f:
            f.write(self.parser_state)  # include parser state if any.
            f.write(code)

        # TODO: grab stderr store in DynaCompilerError
        if dynac(dyna, out):   # stop if the compiler failed.
            raise DynaCompilerError("Failed to compile %r." % dyna)

        return out


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


def dynac(f, out):
    return os.system('%s/dist/build/dyna/dyna -B python -o "%s" "%s"' % (dynahome, out, f))



def dump(code, filename='/tmp/tmp.dyna'):
    "Write code to file."
    with file(filename, 'wb') as f:
        f.write(code)
    return filename


def main():

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

    interp = Interpreter()

    if argv.trace == 'stderr':
        interp.trace = sys.stderr
    elif argv.trace == 'stdout':
        interp.trace = sys.stdout
    else:
        interp.trace = file(argv.trace, 'wb')


    if argv.source:

        if not os.path.exists(argv.source):
            print 'File %r does not exist.' % argv.source
            return

        if argv.plan:
            plan = argv.source
        else:
            plan = "%s.plan.py" % argv.source
            dynac(argv.source, plan)

        interp.do(plan)

        if argv.output:
            if argv.output == "-":
                interp.dump_charts(sys.stdout)
            else:
                with file(argv.output, 'wb') as f:
                    interp.dump_charts(f)
        else:
            interp.dump_charts()

        if argv.interactive:
            interp.repl(hist = argv.source + '.hist')

    else:
        interp.repl(hist = '/tmp/dyna.hist')

    if argv.draw:
        interp.draw()


if __name__ == '__main__':
    main()
