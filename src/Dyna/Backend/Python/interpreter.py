#!/usr/bin/env python

"""
TODO
====

 - dyna syntax which just gets passed to the backend:

 - Use Dyna do some more work! think about using Dyna to maintain rules, update
   handlers, and indices (as Jason points out indices are just memoized
   queries).


FASTER
======

 - specialize calls to emit: don't build the local variable dictionaries if the
   aggregator doesn't use them. Consider generate both versions (or an argument
   to the update handler which will skip the appropriate code paths).

 - faster charts (dynamic argument types? jason's trie data structure)

 - teach planner to prefer not to use the value column, because it's not
   indexed.

 - Collect all query modes used by the planner. Consider indexing value column
   if plans need it.

 - better default prioritization (currently FIFO)

 - BAggregators aren't very efficient.

 - interning with integers instead of deduplicated instances of Term.


STRONGER (robustness)
=====================

 - error handling, some stuff isn't a proper transaction yet.

 - Context manager for disabling ^C in certain blocks.

 - catch compiler errors (for example, ^C while compiling results in a "Compiler
   panic!  This is almost assuredly not your fault!...").


USERS
=====

 - user-defined priorities

 - Catch typos! Warn the user if they write a predicate that is not defined on
   the LHS of a rule and it's not quoted (i.e. not some new piece of structure).
   [mode analysis will help with this].

 - If the solver is taking too long, print an "apology" with some simple
   statistics explaining what the solver is doing (e.g. repropagation-rate: does
   it have a bad prioritization heuristics is it stuck in a cycle; number of
   items proved: is it counting to infinity?).


JUST FOR FUN
============

 - visualize execution of solver on hypergraph -- recreate nwf's animations from
   his ICLP talk.

 - overload everything so that values maintain provenance and we can inspect the
   entire fine-grained circuit.

"""

from __future__ import division
import os, sys, imp, argparse
from collections import defaultdict
from hashlib import sha1
from time import time
from path import path

import load, post

from term import Term, Cons, Nil
from chart import Chart
from utils import ip, red, green, blue, magenta, yellow, parse_attrs, \
    ddict, dynac, read_anf, strip_comments, _repr, hide_ugly_filename

from prioritydict import prioritydict
from config import dotdynadir
from errors import crash_handler, DynaInitializerException, AggregatorError, DynaCompilerError
from stdlib import todyna


class Rule(object):
    def __init__(self, index):
        self.index = index
        self.init = None
        self.updaters = []
        self.query = None
    @property
    def span(self):
        span = parse_attrs(self.init or self.query)['Span']
        return hide_ugly_filename(span)
    @property
    def src(self):
        return strip_comments(parse_attrs(self.init or self.query)['rule'])
    def __repr__(self):
        return 'Rule(%s, %r)' % (self.index, self.src)


# TODO: yuck, hopefully temporary measure to support pickling the Interpreter's
# state
class foo(dict):
    def __init__(self, agg_name):
        self.agg_name = agg_name
        super(foo, self).__init__()
    def __missing__(self, fn):
        arity = int(fn.split('/')[-1])
        self[fn] = c = Chart(fn, arity, self.agg_name[fn])
        return c


def none():
    return None


class Interpreter(object):

    def __init__(self):
        # declarations
        self.agg_name = defaultdict(none)
        self.updaters = defaultdict(list)
        self._gbc = defaultdict(list)

        # data structures
        self.agenda = prioritydict()
        self.parser_state = ''

        self.chart = foo(self.agg_name)
        self.rules = ddict(Rule)
        self.error = {}

        self.files = []

        # interpretor needs a place for it's temporary files.
        self.tmp = tmp = (dotdynadir / 'tmp' / str(os.getpid()))
        if tmp.exists():
            tmp.rmtree()
        tmp.makedirs_p()

    def new_fn(self, fn, agg):
        # check for aggregator conflict.
        if self.agg_name[fn] is None:
            self.agg_name[fn] = agg

            # if we have a new aggregator and an existing chart we need to shove
            # a bunch of aggregators into the interned nodes.
            #
            # This happens when a new rule (e.g. from the repl) gives something
            # a value, which didn't have a value before -- i.e. was only used as
            # structure.
            if fn in self.chart:
                c = self.chart[fn]
                assert c.agg_name is None
                c.agg_name = agg
                for item in c.intern.itervalues():
                    assert item.aggregator is None
                    item.aggregator = c.new_aggregator(item)

        assert self.agg_name[fn] == agg, (fn, self.agg_name[fn], agg)

    def dump_charts(self, out=None):
        if out is None:
            out = sys.stdout
        fns = self.chart.keys()
        fns.sort()
        fns = [x for x in fns if x not in self._gbc]  # don't show backchained items
        nullary = [x for x in fns if x.endswith('/0')]
        others = [x for x in fns if not x.endswith('/0')]

        # show nullary charts first
        nullary = [str(self.chart[x]) for x in nullary]
        charts = [str(self.chart[x]) for x in others if not x.startswith('$rule/')]

        nullary = filter(None, nullary)
        charts = filter(None, charts)

        if nullary or charts:
            print >> out
            print >> out, 'Solution'
            print >> out, '========'
        else:
            print >> out, 'Solution empty.'

        if nullary:
            for line in nullary:
                print >> out, line
        print >> out
        for line in charts:
            print >> out, line
            print >> out

        self.dump_errors(out)

    def dump_errors(self, out=None):
        if out is None:
            out = sys.stdout
        # We only dump the error chart if it's non empty.
        if not self.error:
            return
        print >> out
        print >> out, 'Errors'
        print >> out, '======'

        I = defaultdict(lambda: defaultdict(list))
        E = defaultdict(lambda: defaultdict(list))
        for item, (val, es) in self.error.items():
            for e, h in es:
                if h is None:
                    I[item.fn][type(e)].append((e, item, val))
                else:
                    E[h.rule][type(e)].append((e, item, val))

        # aggregation errors
        for r in sorted(I, key=lambda r: r.index):
            print >> out, 'Error(s) aggregating %s:' % r
            for etype in I[r]:
                print >> out, '  %s:' % etype.__name__
                for i, (e, item, value) in enumerate(sorted(I[r][etype])):                       # todo: probably don't want to show ten million errors
                    if i >= 5:
                        print >> out, '    %s more ...' % (len(I[r][etype]) - i)
                        break
                    print >> out, '    `%s`: %s' % (item, e)
                print >> out

        # errors pertaining to rules
        for r in sorted(E, key=lambda r: r.index):
            print >> out, 'Error(s) in rule:', r.span
            print >> out
            for line in r.src.split('\n'):
                print >> out, '   ', line
            print >> out
            for etype in E[r]:
                print >> out, '  %s:' % etype.__name__
                for i, (e, item, value) in enumerate(sorted(E[r][etype])):                       # todo: probably don't want to show ten million errors
                    if i >= 5:
                        print >> out, '    %s more ...' % (len(E[r][etype]) - i)
                        break
                    print >> out, '    when `%s` = %s' % (item, _repr(value))
                    print >> out, '      %s' % (e)
                print >> out

        print >> out

    def dump_rules(self):
        if not self.rules:
            print 'No rules found.'
            return
        print
        print 'Rules'
        print '====='
        for i in sorted(self.rules):
            print '%3s: %s' % (i, self.rules[i].src)
        print

    def build(self, fn, *args):
        # TODO: codegen should handle true/0 is True and false/0 is False
        if fn == 'true/0':
            return True
        if fn == 'false/0':
            return False
        if fn == 'cons/2':
            return Cons(*args)
        if fn == 'nil/0':
            return Nil

        if fn == '$key/1':
            self.new_fn(fn, '=')

        if fn not in self.agg_name:
            # item has no aggregator and this is the first time we're seeing it.
            self.new_fn(fn, None)

        return self.chart[fn].insert(args)

    def retract_rule(self, idx):
        "Retract rule and all of it's edges."

        try:
            rule = self.rules.pop(idx)
        except KeyError:
            print 'Rule %s not found.' % idx
            return

        # remove $rule
        if hasattr(rule, 'item'):
            self.delete_emit(rule.item, True, ruleix=None, variables=None)

        if rule.init is not None:
            # remove update handlers
            for u in rule.updaters:
                for xs in self.updaters.values():
                    if u in xs:
                        xs.remove(u)
                        assert u not in xs, 'Several occurrences of u in xs'
            # run initializer in delete mode
            try:
                rule.init(emit=self.delete_emit)
            except (TypeError, ZeroDivisionError):
                pass
        else:
            assert rule.query is not None
            # remove query handler
            self._gbc[rule.head_fn].remove(rule.query)
            # blast the memo entries for items it helped derive
            if rule.head_fn in self.chart:
                for head in self.chart[rule.head_fn].intern.itervalues():

                    def _emit(item, val, ruleix, variables):
                        item.aggregator.dec(val, ruleix, variables)

                    try:
                        rule.query(*head.args, emit=_emit)
                    except (TypeError, ZeroDivisionError):
                        pass

                    self.agenda[head] = time()

        return self.go()

    def go(self):
        try:
            return self._go()
        except KeyboardInterrupt:
            print '^C'
            self.dump_charts()

    def _go(self):
        "the main loop"
        changed = {}
        agenda = self.agenda
        error = self.error
        while agenda:

            item = agenda.pop_smallest()
            was = item.value

            try:
                now = item.aggregator.fold()

            except AggregatorError as e:
                error[item] = (None, [(e, None)])

                now = self.build('$error/0')   # XXX: should go an agenda or run delete?
                changed[item] = now
                item.value = now
                continue

            except (ZeroDivisionError, TypeError, KeyboardInterrupt, NotImplementedError) as e:
                error[item] = (None, [(e, None)])

                now = self.build('$error/0')   # XXX: should go an agenda or run delete?
                changed[item] = now
                item.value = now
                continue

            if hasattr(now, 'fn') and now.fn == 'with_key/2':
                now, key = now.args
                dkey = self.build('$key/1', item)
                self.delete_emit(dkey, dkey.value, None, None)
                self.emit(dkey, key, None, None, delete=False)

            if was == now:
                continue

            was_error = False
            if item in error:    # clear error
                was_error = True
                del error[item]

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
        Passes update to relevant handlers. Catches errors.
        """

        # store emissions, make sure all of them succeed before propagating
        # changes to aggregators.
        emittiers = []
        t_emit = lambda item, val, ruleix, variables: \
            emittiers.append((item, val, ruleix, variables, delete))

        error = []

        for handler in self.updaters[item.fn]:
            try:
                handler(item, val, emit=t_emit)
            except (TypeError, ZeroDivisionError, KeyboardInterrupt, OverflowError) as e:
                error.append((e, handler))

        if error:
            self.error[item] = (val, error)
            return

        # no exception, accept emissions.
        for e in emittiers:
            # an error could happen here, but we assume (by contract) that
            # this is not possible.
            self.emit(*e)

    def new_updater(self, fn, ruleix, handler):
        self.updaters[fn].append(handler)
        rule = self.rules[ruleix]
        rule.updaters.append(handler)
        handler.rule = rule

    def gbc(self, fn, *args):
        # TODO: need to distinguish `unknown` from `null`

        head = self.build(fn, *args)

        if head.value is not None:
            return head.value

        head.aggregator.clear()

        def _emit(item, val, ruleix, variables):
            assert item is head, [item, head]
            head.aggregator.inc(val, ruleix, variables)

        for h in self._gbc[fn]:
            h(*args, emit=_emit)

        head.value = head.aggregator.fold()
        return head.value

    def new_query(self, fn, ruleix, handler):
        self._gbc[fn].append(handler)
        rule = self.rules[ruleix]
        assert rule.query is None
        rule.query = handler
        handler.rule = rule
        rule.head_fn = fn

    def new_initializer(self, ruleix, init):
        rule = self.rules[ruleix]
        assert rule.init is None
        rule.init = init
        init.rule = rule

    def delete_emit(self, item, val, ruleix, variables):
        self.emit(item, val, ruleix, variables, delete=True)

    def emit(self, item, val, ruleix, variables, delete): #, aggregator_to_inherit=None):
        if delete:
            item.aggregator.dec(val, ruleix, variables)
        else:
            item.aggregator.inc(val, ruleix, variables)
        self.agenda[item] = time()  # FIFO

    def do(self, filename, initialize=True):
        """
        Compile, load, and execute new dyna rules.

        To support the REPL, we try do load these new rules in a transaction --
        if any rule in the newly loaded code is "bad," we simple reject the
        addition of all these the new rules.

        A rule is bad if the compiler rejects it or it's initializer fails.
        """
        assert os.path.exists(filename)

        env = imp.load_source('dynamically_loaded_module', filename)

        if path(filename + '.anf').exists():       # XXX: should have codegen provide this in plan.py
            with file(filename + '.anf') as f:
                for anf in read_anf(f.read()):
                    self.rules[anf.ruleix].anf = anf

        for k,v in [('chart', self.chart),
                    ('build', self.build),
                    ('gbc', self.gbc),
                    ('peel', peel)]:
            setattr(env, k, v)

        emits = []
        def _emit(*args):
            emits.append(args)

        # TODO: this should be a transaction.
        for k, v in env.agg_decl.items():
            self.new_fn(k, v)

        new_rules = set()
        for _, r, _ in env.queries:
            new_rules.add(r)
        for r, _ in env.initializers:
            new_rules.add(r)
        self.new_rules = new_rules

        for fn, r, h in env.queries:
            self.new_query(fn, r, h)

        try:
            if initialize:
                # only run new initializers
                for _, init in env.initializers:
                    init(emit=_emit)

        except (TypeError, ZeroDivisionError) as e:
            raise DynaInitializerException(e, init)

        else:
            for fn, r, h in env.updaters:
                self.new_updater(fn, r, h)
            for r, h in env.initializers:
                self.new_initializer(r, h)

            # accept the new parser state
            self.parser_state = env.parser_state

            # process emits
            for e in emits:
                self.emit(*e, delete=False)

        # ------ $rule for fun and profit -------
        interp = self
        def rule(ix, *a):
            fn = '$rule/%s' % (len(a) + 1)
            if interp.agg_name[fn] is None:
                interp.new_fn(fn, ':=')
            item = interp.build(fn, ix, *a)
            interp.emit(item, True, ruleix=None, variables=None, delete=False)
            return item
        for i in new_rules:
            r = self.rules[i]
            if hasattr(r, 'anf'):   # XXX: all rules should have ANF!
                agg, head, evals, unifs, result = r.anf[2:]
                r.item = rule(i, r.src, todyna([head, agg, result, evals, unifs]), r.init, r.query)
        #-----------------------------------------

        return self.go()

    def dynac(self, filename):
        filename = path(filename)
        self.files.append(filename)
        out = self.tmp / filename.read_hexhash('sha1') + '.plan.py'
#        out = filename + '.plan.py'
        self.files.append(out)
        dynac(filename, out)
        return out

    def dynac_code(self, code):
        """
        Compile a string of dyna code.

        raises ``DynaCompilerError``
        """
        x = sha1()
        x.update(self.parser_state)
        x.update(code)

        dyna = self.tmp / ('%s.dyna' % x.hexdigest())

        with file(dyna, 'wb') as f:
            f.write(self.parser_state)  # include parser state if any.
            f.write(code)

        return self.dynac(dyna)


def peel(fn, item):
    """
    Find item's args in the appropriate chart. Assert that idx matches
    functor/arity, `fn`. Returns the arguments of term as a tuple of intern idxs
    and constants (possibly an empty tuple).
    """
    if fn == "true/0":
        assert item is True
        return
    if fn == "false/0":
        assert item is False
        return
    assert isinstance(item, Term)
    assert item.fn == fn
    return item.args


def main():
    parser = argparse.ArgumentParser(description="The dyna interpreter!")
    parser.add_argument('source', nargs='*', type=path,
                        help='Path to Dyna source file (or plan if --plan=true).')
    parser.add_argument('-i', dest='interactive', action='store_true',
                        help='Fire-up REPL after runing solver..')
    parser.add_argument('--plan', action='store_true',
                        help='`source` specifies output of the compiler instead of dyna source code.')
    parser.add_argument('-o', '--output', dest='output',
                        type=argparse.FileType('wb'),
                        help='Write solution to file.')
    parser.add_argument('--post-process', nargs='*',
                        help='run post-processor.')
    parser.add_argument('--load', nargs='*',
                        help='run loaders.')
#    parser.add_argument('--profile', action='store_true',
#                        help='run profiler.')

    args = parser.parse_args()

    interp = Interpreter()

    crash_handler()

    if args.source:

        if len(args.source) > 1:
            # concatenate files
            with file(interp.tmp / 'tmp.dyna', 'wb') as g:
                for f in args.source:
                    if not os.path.exists(f):
                        print 'File %r does not exist.' % f
                    with file(f) as f:
                        g.write('\n')
                        g.write('%'*80)
                        g.write('\n')
                        g.write('%% ')
                        g.write(f.name)
                        g.write('\n')
                        g.write(f.read())
            args.source = g.name
        else:
            [args.source] = args.source

        if not os.path.exists(args.source):
            print 'File %r does not exist.' % args.source
            return

        if args.plan:
            # copy plan to tmp directory
            plan = interp.tmp / args.source.read_hexhash('sha1') + '.plan.py'
            args.source.copy(plan)

        else:
            try:
                plan = interp.dynac(args.source)
            except DynaCompilerError as e:
                print e
                exit(1)

#        if args.profile:
#            # When profiling, its common practice to disable the garbage collector.
#            import gc
#            gc.disable()
#
#            from cProfile import Profile
#            p = Profile()
#            p.runctx('interp.do(plan)', globals(), locals())
#            p.dump_stats('prof')
#
#            interp.dump_charts()
#
#            os.system('gprof2dot.py -f pstats prof | dot -Tsvg -o prof.svg && eog prof.svg &')
#            os.system('pkill snakeviz; snakeviz prof &')
#            return

        try:
            interp.do(plan)
        except DynaInitializerException as e:
            print e
            exit(1)

    if args.load:
        for cmd in args.load:
            load.run(interp, cmd)

    if args.post_process:
        for cmd in args.post_process:
            post.run(interp, cmd)

    if args.load or args.post_process or args.source:
        interp.dump_charts(args.output)      # should be a post-processor

    if args.interactive or not args.source:
        from repl import REPL
        repl = REPL(interp)

        def repl_crash():
            # all files the interpreter generated
            with file(dotdynadir / 'crash-repl.log', 'wb') as f:
                for line in repl.lines:
                    print >> f, line

        crash_handler.hooks.append(repl_crash)

        repl.cmdloop()


if __name__ == '__main__':
    main()
