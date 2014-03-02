#!/usr/bin/env python

import re, os, sys, imp, traceback
from collections import defaultdict

from term import Term, Cons, Nil, MapsTo, Error
from chart import Chart
from utils import red, ANF, span_to_src, _repr, hide_ugly_filename, \
    true, false, magenta, indent, path

from prioritydict import prioritydict
from config import dotdynadir
from errors import rule_error_context, AggregatorError, DynaCompilerError
from stdlib import todyna
from dynac import Compiler

#sys.setrecursionlimit(10000)


class Rule(object):

    def __init__(self, index):
        self.index = index
        self.init = None
        self.updaters = []
        self.query = None
        self.span = None
        self.src = None
        self.initialized = False
        self.anf = None
        self.head_fn = None

    def __eq__(self, other):
        return self.index == other.index

    def hash(self):
        return self.index

    def __cmp__(self, other):
        try:
            return cmp(self.index, other.index)
        except AttributeError:
            return 1

    def __repr__(self):
        return '%3s: %s' % (self.index, self.src)

    def render_ctx(self, ctx, indent=''):
        # TODO: highlight expression which caused the error.
        from post.trace import Crux
        c = Crux(head=None, rule=self, body=None, vs = ctx)
        return '\n'.join(indent + line for line in c.format())


class Charts(dict):
    def __init__(self, agg_name):
        self.agg_name = agg_name
        super(Charts, self).__init__()
    def __missing__(self, fn):
        arity = int(fn.split('/')[-1])
        self[fn] = c = Chart(fn, arity, self.agg_name[fn])
        return c


def none():
    return None


class Interpreter(object):

    def __init__(self):
        self.compiler = Compiler()
        # parser state
        self.agg_name = defaultdict(none)
        self.bc = set()
        self.other = []
        self.ruleix = 0
        # rules
        self.rules = {}
        self.updaters = defaultdict(list)
        self._gbc = defaultdict(list)
        # data structures
        self.agenda = prioritydict()
        self.chart = Charts(self.agg_name)
        self.error = {}
        self.changed = {}
        # misc
        self.time_step = 0
        # coarsening of the program shows which rules might depend on each other
        self.coarse_deps = defaultdict(set)
        self.rule_by_head = defaultdict(set)
        self.rule_dep = defaultdict(set)        # rules which depend on a predicate
        # rules which need to be recompiled against new program state.
        self.recompile = set()
        # forward chaining rules which failed to initialize
        self.uninitialized_rules = []

    def new_fn(self, fn, agg):
        if self.agg_name[fn] is None:
            self.agg_name[fn] = agg
            self.chart[fn].set_aggregator(agg)
        # check for aggregator conflict.
        assert self.agg_name[fn] == agg

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
        if fn == 'true/0':
            return true
        if fn == 'false/0':
            return false
        return self.chart[fn].insert(args)

    def delete(self, item, val, ruleix, variables):
        self.clear_error(item)
        self.emit(item, val, ruleix, variables, delete=True)

    def emit(self, item, val, ruleix, variables, delete):
        if delete:
            item.aggregator.dec(val, ruleix, variables)
        else:
            item.aggregator.inc(val, ruleix, variables)
        self.agenda[item] = self.time_step
        self.time_step += 1

    def run_agenda(self):
        self.changed = {}
        try:
            self._agenda()
        except KeyboardInterrupt:
            print '^C'
        return self.changed

    def _agenda(self):
        "the main loop"
        agenda = self.agenda
        while agenda:
            item = self.agenda.pop_smallest()
            self.pop(item)
        self.run_recompile()
        self.run_uninitialized()
        if self.agenda:
            self._agenda()

    def pop(self, item):
        """
        Handle popping `item`: fold `item`'s aggregator to get it's new value
        (handle errors), propagate changes to the rest of the circuit.
        """
        try:
            # compute item's new value
            now = item.aggregator.fold()
        except (AggregatorError, ZeroDivisionError, ValueError, TypeError, OverflowError) as e:
            # handle error in aggregator
            now = Error()
            self.replace(item, now)
            self.set_error(item, (None, [(e, None)]))
        else:
            self.replace(item, now)
        return now

    def replace(self, item, now):
        "replace current value of ``item``, propagate any changes."
        was = item.value
        if was == now:
            # nothing to do.
            return
        # special handling for with_key, forks a second update
        k = self.build('$key/1', item)
        if hasattr(now, 'fn') and now.fn == 'with_key/2':
            now, key = now.args
            self.replace(k, key)
            if was == now:
                return
        else:
            # retract $key when we retract the item or no longer have a with_key
            # as the value.
            if k.value is not None:
                self.replace(k, None)
        # delete existing value before so we can replace it
        if was is not None:
            self.push(item, was, delete=True)
        # clear existing errors -- we only care about errors at new value.
        self.clear_error(item)
        # new value enters in the chart.
        item.value = now
        # push changes
        if now is not None:
            self.push(item, now, delete=False)
        # make note of change
        self.changed[item] = now

    def push(self, item, val, delete):
        """
        Passes update to relevant handlers. Catches errors.
        """

        # store emissions, make sure all of them succeed before propagating
        # changes to aggregators.
        emittiers = []
        t_emit = lambda item, val, ruleix, variables: \
           emittiers.append((item, val, ruleix, variables, delete))

        errors = []

        for handler in self.updaters[item.fn]:

            # TODO: should only add update handlers after rule has been initialized.
            if not handler.rule.initialized:
                continue

            try:
                handler(item, val, emit=t_emit)
            except (ZeroDivisionError, ValueError, TypeError, RuntimeError, OverflowError) as e:
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()
                errors.append((e, handler))

        if errors:
            self.set_error(item, (val, errors))
            return

        # no exceptions, accept emissions.
        for e in emittiers:
            # an error could happen here, but we assume (by contract) that this
            # is not possible.
            self.emit(*e)

    def gbc(self, fn, args):
        item = self.build(fn, *args)
        if item.value is not None:
            return item.value
        return self.force_gbc(item)

    def force_gbc(self, item):
        "Skips memo on item check."

        if item.aggregator is None:   # we might not have a rule defining this subgoal.
            return

        self.clear_error(item)

        item.aggregator.clear()

        emits = []
        def t_emit(item, val, ruleix, variables):
            emits.append((item, val, ruleix, variables, False))

        errors = []
        for handler in self._gbc[item.fn]:
            try:
                handler(*item.args, emit=t_emit)
            except (ZeroDivisionError, ValueError, TypeError, RuntimeError, OverflowError) as e:
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()
                errors.append((e, handler))

        if hasattr(self, 'was') and self.was:
            was = self.was
            if item in was:
                if item.value != was[item]:
                    item.value = was[item]

        if errors:
            e = Error()
            self.push(item, e, delete=False)

            # Force value to error, since aggregator might ignore it. This is
            # not correct XREF:agg-error
            self.replace(item, e)
            self.set_error(item, (None, errors))
            return e
        else:
            for e in emits:
                self.emit(*e)
            return self.pop(item)

    def load_plan(self, filename):
        """
        Compile, load, and execute new dyna rules.

        To support the REPL, we try do load these new rules in a transaction --
        if any rule in the newly loaded code is "bad," we simple reject the
        addition of all these the new rules.
        """
        assert os.path.exists(filename)

        env = imp.load_source('dynamically_loaded_module', filename)

        for k,v in [('chart', self.chart),
                    ('build', self.build),
                    ('gbc', self.gbc),
                    ('peel', peel)]:
            setattr(env, k, v)

        # update parser state
        self.set_parser_state(env.parser_state)

        for k, v in env.agg_decl.items():
            self.new_fn(k, v)

        new_rules = set()
        for _, index, h, span, anf in env.queries:
            new_rules.add(index)
            self.add_rule(index, span, ANF.read(span, index, anf), query=h)

        for index, h, span, anf in env.initializers:
            new_rules.add(index)
            self.add_rule(index, span, ANF.read(span, index, anf), init=h)

        for fn, r, h in env.updaters:
            self.new_updater(fn, r, h)

        return new_rules

    def set_parser_state(self, x):
        (bc, _rix, _agg, _other) = self.compiler.read_parser_state(x)
        # update misc parser state
        self.other = _other
        # update backchain predicates
        for fn in bc:
            self.bc.add(fn)
            if fn not in self._gbc:    # new backchain declaration
                for r in self.rule_by_head[fn]:
                    self.needs_recompile(r)
        # update ruleix
        if not self.rules:
            self.ruleix = _rix
        else:
            self.ruleix = max(max(self.rules) + 1, _rix)   # next available
        # update fn->aggregator map
        for fn, agg in _agg.items():
            self.new_fn(fn, agg)

    def recompile_rule(self, r):
        "returns a plan, it's up to you to retract the old rule and load the plan"
        pstate = self.compiler.parser_state(self.bc, r.index, self.agg_name, self.other)   # override ruleix
        return self.compiler.dynac_code(r.src, pstate)

    def needs_recompile(self, r):
        self.retract_rule(r.index)   # clears errors
        self.recompile.add(r)

    def run_recompile(self):
        # run to fixed point.
        while self.recompile:
            success = set()
            failure = set()
            for r in list(self.recompile):
                try:
                    r.plan = self.recompile_rule(r)
                except DynaCompilerError as e:
                    failure.add(r)
                    self.set_error(r, e)
                else:
                    success.add(r)
                    self.clear_error(r)
            self.recompile = failure
            if not success:
                break
            for r in success:
                self.load_plan(r.plan)

    def run_uninitialized(self):
        q = set(self.uninitialized_rules)
        failed = []
        # run to fixed point
        while q:
            rule = q.pop()
            try:
                assert not rule.initialized
                emits = []
                def _emit(*args):
                    emits.append(args)
                self.clear_error(rule)  # clear errors on rule, if any
                rule.init(emit=_emit)
            except (ZeroDivisionError, ValueError, TypeError, RuntimeError, OverflowError) as e:
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()
                self.set_error(rule, e)
                failed.append(rule)
            else:
                rule.initialized = True
                for e in emits:
                    self.emit(*e, delete=False)
        self.uninitialized_rules = failed

    #___________________________________________________________________________
    # Error tracking

    def clear_error(self, x):
        if x in self.error:
            del self.error[x]

    def set_error(self, x, e):
        self.error[x] = e

    #___________________________________________________________________________
    # Adding/removing rules

    def add_rule(self, index, span, anf, init=None, query=None):
        assert index not in self.rules

        for (x, label, ys) in anf.unifs:
            if x == anf.head:
                assert label == '&'
                head_fn = '%s/%d' % (ys[0], len(ys) - 1)
                break
        else:
            raise AssertionError('Did not find head.')

        self.rules[index] = rule = Rule(index)
        rule.span = hide_ugly_filename(span)
        rule.src = span_to_src(span).strip()
        rule.anf = anf
        rule.head_fn = head_fn

        self.update_coarse(rule)

        if init:
            rule.init = init
            init.rule = rule
            self.uninitialized_rules.append(rule)

        elif query:
            rule.query = query
            query.rule = rule

            # fix dependents
            if head_fn not in self._gbc:
                # retract and replan rules dependent on this predicate which is
                # now backchained.
                for d in self.rule_dep[head_fn]:
                    if rule != d and d.head_fn not in self._gbc:
                        self.needs_recompile(d)

            self._gbc[head_fn].append(query)

            if head_fn in self.chart:
                # if we've added a new rule we need to recompute memos for
                # existing memos. (TODO: can do slightly better than recompute
                # -- only need to evaluate contributions from this new rule)
                self.recompute_gbc_memo(head_fn)

        else:
            raise AssertionError("Can't add rule with out an initializer or query handler.")

        if True:
            args = (index,
                    rule.src,
                    todyna([anf.head, anf.agg, anf.result, anf.evals, anf.unifs]),
                    init,
                    query)
            fn = '$rule/%s' % len(args)
            if self.agg_name[fn] is None:
                self.new_fn(fn, ':=')
            rule.item = self.build(fn, *args)
            self.emit(rule.item, true, ruleix=None, variables=None, delete=False)

    def update_coarse(self, rule):
        self.rule_by_head[rule.head_fn].add(rule)
        for (_, label, ys) in rule.anf.evals:
            [(label, _evalix)] = re.findall('^(.*)@(\d+)$', label)  # remove evaluation index
            b = '%s/%d' % (label, len(ys))
            self.coarse_deps[b].add(rule.head_fn)
            self.rule_dep[b].add(rule)

    def recompute_coarse(self):
        self.rule_by_head.clear()
        self.coarse_deps.clear()
        self.rule_dep.clear()
        for rule in self.rules.values():
            self.update_coarse(rule)

    def retract_rule(self, idx):
        "Retract rule and all of it's edges."

        self.changed = {}

        try:
            rule = self.rules.pop(idx)
        except KeyError:
            print 'Rule %s not found.' % idx
            return

        self.clear_error(rule)

        # remove $rule
        if hasattr(rule, 'item'):
            self.delete(rule.item, true, ruleix=None, variables=None)

        if rule.init is not None:
            # Forward chained rule --
            # remove update handlers
            for u in rule.updaters:
                for xs in self.updaters.values():
                    if u in xs:
                        xs.remove(u)
            if rule.initialized:
                # run initializer in delete mode
                try:
                    rule.init(emit=self.delete)
                except (ZeroDivisionError, ValueError, TypeError, RuntimeError, OverflowError):
                    # TODO: what happens if there's an error?
                    pass
            else:
                self.uninitialized_rules.remove(rule)

        else:

            # Backchained rule --
            # remove query handler
            self._gbc[rule.head_fn].remove(rule.query)

            # recompute memos and dependent memos
            self.recompute_gbc_memo(rule.head_fn)

        # clear push-time errors pertaining to this rule
        for item, x in self.error.items():
            if isinstance(item, Rule):
                continue
            (v, es) = x
            self.error[item] = (v, [(e, h) for e, h in es if h is None or h.rule == rule])

        self.recompute_coarse()

        self._agenda()

        # if there are no more rules defining a functor; clear some of the state
        if not self.rule_by_head[rule.head_fn]:
            if rule.head_fn in self.chart:
                self.chart[rule.head_fn].set_aggregator(None)
            #if rule.head_fn in self.agg_name: rule can't exist with out an aggregator
            del self.agg_name[rule.head_fn]

        return self.changed

    def recompute_gbc_memo(self, fn, visited=None):

        if visited is None:
            visited = set()

        if fn not in self.bc or fn in visited:
            # don't refresh non-BC computation. Also, be careful not to get
            # stuck in an infinite loop if there is a cycle in the dep graph
            return

        visited.add(fn)

        # YUCK: find some was to avoid this...  alternatively, we can run
        # deletes first.
        self.was = {x: x.value for x in self.chart[fn].intern.values()}

        for x in self.chart[fn].intern.values():
            x.value = None       # avoid memo

        for x in self.chart[fn].intern.values():
            now = self.force_gbc(x)

            if now != self.was[x]:
                self.changed[x] = now
            else:
                if x in self.changed:
                    del self.changed[x]

        self.was = None

        # recompute dependent BC memos
        for v in self.coarse_deps[fn]:
            self.recompute_gbc_memo(v, visited)

    def new_updater(self, fn, ruleix, handler):
        self.updaters[fn].append(handler)
        rule = self.rules[ruleix]
        rule.updaters.append(handler)
        handler.rule = rule

    #___________________________________________________________________________
    # Communication with Dyna compiler

    def dynac(self, filename):
        "Compile a string of dyna code."
        return self.compiler.dynac(filename)

    def dynac_code(self, code):
        "Compile a string of dyna code."
        pstate = self.compiler.parser_state(self.bc, self.ruleix, self.agg_name, self.other)
        return self.compiler.dynac_code(code, pstate)

    #___________________________________________________________________________
    # Routines for showing things to the user.

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

        # separate errors into aggregation errors and update handler errors
        I = defaultdict(lambda: defaultdict(list))
        E = defaultdict(lambda: defaultdict(list))
        for item, x in self.error.items():
            if isinstance(item, Rule):
                continue
            (val, es) = x
            for e, h in es:
                if h is None:
                    I[item.fn][type(e)].append((item, e))
                else:
                    assert h.rule.index in self.rules
                    E[h.rule][type(e)].append((item, val, e))

        # We only dump the error chart if it's non empty.
        if not I and not E and not self.uninitialized_rules and not self.recompile:
            return

        print >> out
        print >> out, red % 'Errors'
        print >> out, red % '======'

        # aggregation errors
        for fn in sorted(I):
            print >> out, 'Error(s) aggregating %s:' % fn
            for etype in I[fn]:
                print >> out, '  %s:' % etype.__name__
                for i, (item, e) in enumerate(sorted(I[fn][etype])):
                    if i >= 5:
                        print >> out, '    %s more ...' % (len(I[fn][etype]) - i)
                        break
                    print >> out, '    `%s`: %s' % (item, e)
                print >> out

        # errors pertaining to rules
        for r in sorted(E):
            print >> out, 'Error(s) in rule %s:' % r.index, r.span
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

                    if 'maximum recursion depth exceeded' in str(e):
                        # simplify recurision limit error because it prints some
                        # unpredictable stuff.
                        print >> out, '      maximum recursion depth exceeded'
                    else:
                        print >> out, '      %s' % (e)

                    #print >> out
                    #print >> out, magenta % indent(e.traceback.rstrip(), indent='        ')

                    print >> out
                    print >> out, r.render_ctx(e.exception_frame, indent='      ')
                    print >> out

        # uninitialized rules
        if self.uninitialized_rules:
            print >> out, red % 'Uninitialized rules'
            print >> out, red % '==================='
            for rule in sorted(self.uninitialized_rules):
                e = self.error[rule]
                print >> out, 'Failed to initialize rule:'
                print >> out, '   ', rule.src
                print >> out, '  due to `%s`' % e
                print >> out, rule.render_ctx(e.exception_frame, indent='    ')
                print >> out

        # rules which failed to recompile
        if self.recompile:
            print >> out, red % 'Failed to recompile'
            print >> out, red % '==================='
            for rule in sorted(self.recompile):
                e = self.error[rule]
                print >> out, 'Failed to recompile rule:'
                print >> out, '   ', rule.src
                print >> out, '  with error'
                for line in str(e).split('\n'):
                    print >> out, '   ', line
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
                print '%s  <-- uninitialized' % rule
            else:
                print rule
        print


def peel(fn, item):
    """
    Find item's args in the appropriate chart. Assert that idx matches
    functor/arity, `fn`. Returns the arguments of term as a tuple of intern idxs
    and constants (possibly an empty tuple).
    """
    assert isinstance(item, Term) and  item.fn == fn
    return item.args
