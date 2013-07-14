#!/usr/bin/env python

import os, sys, imp, traceback
from collections import defaultdict
from hashlib import sha1
from path import path

from term import Term, Cons, Nil, MapsTo
from chart import Chart
from utils import red, parse_attrs, ddict, dynac, read_anf, strip_comments, \
    _repr, hide_ugly_filename, true, false

from prioritydict import prioritydict
from config import dotdynadir
from errors import rule_error_context, AggregatorError, DynaCompilerError
from stdlib import todyna


#sys.setrecursionlimit(10000)


class Rule(object):

    def __init__(self, index):
        self.index = index
        self.init = None
        self.updaters = []
        self.query = None
        self._span = None
        self._src = None
        self.initialized = False

    @property
    def span(self):
        if self._span is None:
            span = parse_attrs(self.init or self.query)['Span']
            self._span = hide_ugly_filename(span)
        return self._span

    @property
    def src(self):
        if self._src is None:
            self._src = strip_comments(parse_attrs(self.init or self.query)['rule'])
        return self._src

    def __repr__(self):
        return 'Rule(%s, %r)' % (self.index, self.src)

    def render_ctx(self, ctx, indent=''):
        # TODO: highlight expression which caused the error.
        from post.trace import Crux
        c = Crux(head=None, rule=self, body=None, vs = ctx)
        return '\n'.join(indent + line for line in c.format())


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
        self.parser_state = ''
        self.files = []
        # rules
        self.rules = ddict(Rule)
        self.updaters = defaultdict(list)
        self._gbc = defaultdict(list)
        # data structures
        self.agenda = prioritydict()
        self.chart = foo(self.agg_name)
        self.error = {}
        self.uninitialized_rules = []
        # misc
        self.time_step = 0
        # interpretor needs a place for it's temporary files.
        self.tmp = tmp = (dotdynadir / 'tmp' / str(os.getpid()))
        if tmp.exists():
            tmp.rmtree()
        tmp.makedirs_p()

    def new_fn(self, fn, agg):
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
        # check for aggregator conflict.
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
        nullary = filter(None, [str(self.chart[x]) for x in nullary])
        charts = filter(None, [str(self.chart[x]) for x in others if not x.startswith('$rule/')])

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
        if not self.error and not self.uninitialized_rules:
            return
        print >> out
        print >> out, red % 'Errors'
        print >> out, red % '======'

        # separate errors into aggregation errors and update handler errors
        I = defaultdict(lambda: defaultdict(list))
        E = defaultdict(lambda: defaultdict(list))
        for item, (val, es) in self.error.items():
            for e, h in es:
                if h is None:
                    I[item.fn][type(e)].append((item, val, e))
                else:
                    E[h.rule][type(e)].append((item, val, e))

        # aggregation errors
        for r in sorted(I, key=lambda r: r.index):
            print >> out, 'Error(s) aggregating %s:' % r
            for etype in I[r]:
                print >> out, '  %s:' % etype.__name__
                for i, (item, value, e) in enumerate(sorted(I[r][etype])):
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
                for i, (item, value, e) in enumerate(sorted(E[r][etype])):
                    if i >= 5:
                        print >> out, '    %s more ...' % (len(E[r][etype]) - i)
                        break
                    print >> out, '    when `%s` = %s' % (item, _repr(value))
                    print >> out, '      %s' % (e)
                    print >> out
                    print >> out, r.render_ctx(e.exception_frame, indent='      ')
                    print >> out

        # uninitialized rules
        if self.uninitialized_rules:
            print >> out, red % 'Uninitialized rules'
            print >> out, red % '==================='
            for e, r in self.uninitialized_rules:
                print >> out, 'Failed to initialize rule:'
                rule = self.rules[r]
                print >> out, '   ', rule.src
                print >> out, '  due to `%s`' % e
                print >> out, rule.render_ctx(e.exception_frame, indent='    ')
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
            rule = self.rules[i]
            if rule.init is not None and not rule.initialized:
                print '%3s: %s  <-- uninitialized' % (i, rule.src)
            else:
                print '%3s: %s' % (i, rule.src)
        print

    def build(self, fn, *args):
        # handle a few special cases where the item doesn't have a chart
        if fn == 'cons/2':
            return Cons(*args)
        if fn == 'nil/0':
            return Nil
        if fn == '->/2':
            return MapsTo(*args)
        if fn == '$key/1':
            self.new_fn(fn, '=')
        # if we haven't seen this functor before, it probably doesn't have a
        # Chart, so lets go ahead and create one.
        if fn not in self.agg_name:
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
            self.delete_emit(rule.item, true, ruleix=None, variables=None)

        uninits = [(e, r) for e, r in self.uninitialized_rules if r != idx]
        if len(uninits) != len(self.uninitialized_rules):
            self.uninitialized_rules = uninits
            return []

        if rule.init is not None:
            # Forward chained rule --
            # remove update handlers
            for u in rule.updaters:
                for xs in self.updaters.values():
                    if u in xs:
                        xs.remove(u)
                        assert u not in xs, 'Several occurrences of u in xs'
            if rule.initialized:
                # run initializer in delete mode
                try:
                    rule.init(emit=self.delete_emit)
                except (ZeroDivisionError, TypeError, KeyboardInterrupt, RuntimeError, OverflowError):
                    # TODO: what happens if there's an error?
                    pass
        else:
            # Backchained rule --
            # remove query handler
            self._gbc[rule.head_fn].remove(rule.query)
            # blast the memo entries for items this rule may have helped derive.
            if rule.head_fn in self.chart:

                # update values before propagating
                for head in self.chart[rule.head_fn].intern.itervalues():
                    def _emit(item, val, ruleix, variables):
                        item.aggregator.dec(val, ruleix, variables)
                    try:
                        rule.query(*head.args, emit=_emit)
                    except (ZeroDivisionError, TypeError, KeyboardInterrupt, RuntimeError, OverflowError):
                        # TODO: what happens if there's an error?
                        pass

                # propagate new values
                for head in self.chart[rule.head_fn].intern.itervalues():
                    self.agenda[head] = self.time_step
                    self.time_step += 1

        return self.go()

    def go(self):
        try:
            return self._go()
        except KeyboardInterrupt:
            print '^C'
            self.dump_charts()

    def _go(self, changed=None):
        "the main loop"
        if changed is None:
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

            except (ZeroDivisionError, TypeError, KeyboardInterrupt, OverflowError) as e:
                error[item] = (None, [(e, None)])

                now = self.build('$error/0')   # XXX: should go an agenda or run delete?
                changed[item] = now
                item.value = now
                continue

            # special handling for with_key, forks into two updates
            if hasattr(now, 'fn') and now.fn == 'with_key/2':
                now, key = now.args
                dkey = self.build('$key/1', item)
                self.delete_emit(dkey, dkey.value, None, None)
                self.emit(dkey, key, None, None, delete=False)

            if was == now:
                continue

            # TODO: handle `was` and `now` at the same time to avoid the two passes.
            # TODO: will need to propagate was=None when we have question mark
            if was is not None: # and item not in error:
                # if `was` is marked as an error we know it didn't propagate.
                # Thus, we can skip the delete-updates.
                self.update_dispatcher(item, was, delete=True)

            if item in error:     # only care about errors at new value.
                del error[item]

            item.value = now

            if now is not None:
                self.update_dispatcher(item, now, delete=False)

            changed[item] = now

        if self.uninitialized_rules:
            self.run_uninitialized()

            if self.agenda:
                self._go(changed)

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

            # TODO: should only add update handlers after rule has been initialized.
            if not handler.rule.initialized:
                continue

            try:
                handler(item, val, emit=t_emit)
            except (ZeroDivisionError, TypeError, KeyboardInterrupt, RuntimeError, OverflowError) as e:
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()
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

        if head.aggregator is None:   # we might not have a rule defining this subgoal.
            return

        head.aggregator.clear()

        def _emit(item, val, ruleix, variables):
            item.aggregator.inc(val, ruleix, variables)

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

    def emit(self, item, val, ruleix, variables, delete):
        if delete:
            item.aggregator.dec(val, ruleix, variables)
        else:
            item.aggregator.inc(val, ruleix, variables)
        self.agenda[item] = self.time_step
        self.time_step += 1

    def do(self, filename):
        """
        Compile, load, and execute new dyna rules.

        To support the REPL, we try do load these new rules in a transaction --
        if any rule in the newly loaded code is "bad," we simple reject the
        addition of all these the new rules.
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

        for k, v in env.agg_decl.items():
            self.new_fn(k, v)

        new_rules = set()
        for fn, r, h in env.queries:
            self.new_query(fn, r, h)
            new_rules.add(r)
        for fn, r, h in env.updaters:
            self.new_updater(fn, r, h)
        for r, h in env.initializers:
            self.new_initializer(r, h)
            new_rules.add(r)
            self.uninitialized_rules.append((None, r))
        self.new_rules = new_rules

        # accept the new parser state
        self.parser_state = env.parser_state

        # ------ $rule for fun and profit -------
        interp = self
        def rule(ix, *a):
            fn = '$rule/%s' % (len(a) + 1)
            if interp.agg_name[fn] is None:
                interp.new_fn(fn, ':=')
            item = interp.build(fn, ix, *a)
            interp.emit(item, true, ruleix=None, variables=None, delete=False)
            return item
        for i in new_rules:
            r = self.rules[i]
            if hasattr(r, 'anf'):   # XXX: all rules should have ANF!
                agg, head, evals, unifs, result = r.anf[2:]
                r.item = rule(i, r.src, todyna([head, agg, result, evals, unifs]), r.init, r.query)
        #-----------------------------------------

        self.run_uninitialized()

        return self.go()

    def run_uninitialized(self):

        q = list(self.uninitialized_rules)
        failed = []

        self.uninitialized_rules = []

        while q:
            (_, r) = q.pop()
            try:

                rule = self.rules[r]
                assert not rule.initialized

                emits = []
                def _emit(*args):
                    emits.append(args)

                rule.init(emit=_emit)

            except (ZeroDivisionError, TypeError, KeyboardInterrupt, RuntimeError, OverflowError) as e:
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()
                failed.append((e, r))

            else:
                rule.initialized = True
                # process emits
                for e in emits:
                    self.emit(*e, delete=False)

        self.uninitialized_rules = failed

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
    assert isinstance(item, Term)
    assert item.fn == fn
    return item.args
