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
        self.span = None
        self.src = None
        self.initialized = False
        self.anf = None
        self.head_fn = None

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
        self.rules = {}
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

    def delete_emit(self, item, val, ruleix, variables):
        self.emit(item, val, ruleix, variables, delete=True)

    def emit(self, item, val, ruleix, variables, delete):
        if delete:
            item.aggregator.dec(val, ruleix, variables)
        else:
            item.aggregator.inc(val, ruleix, variables)
        self.agenda[item] = self.time_step
        self.time_step += 1

    def run_agenda(self, changed=None):
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
                self.run_agenda(changed)

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

    def load_plan(self, filename):
        """
        Compile, load, and execute new dyna rules.

        To support the REPL, we try do load these new rules in a transaction --
        if any rule in the newly loaded code is "bad," we simple reject the
        addition of all these the new rules.
        """
        assert os.path.exists(filename)

        env = imp.load_source('dynamically_loaded_module', filename)

        anf = {}
        if path(filename + '.anf').exists():       # XXX: should have codegen provide this in plan.py
            with file(filename + '.anf') as f:
                for x in read_anf(f.read()):
                    anf[x.ruleix] = x

        for k,v in [('chart', self.chart),
                    ('build', self.build),
                    ('gbc', self.gbc),
                    ('peel', peel)]:
            setattr(env, k, v)

        for k, v in env.agg_decl.items():
            self.new_fn(k, v)

        new_rules = set()
        for fn, index, h in env.queries:
            new_rules.add(index)
            self.add_rule(index, query=h, head_fn=fn, anf=anf[index])

        for index, h in env.initializers:
            new_rules.add(index)
            self.add_rule(index, init=h, anf=anf[index])

        for fn, r, h in env.updaters:
            self.new_updater(fn, r, h)

        # accept the new parser state
        self.parser_state = env.parser_state

        return new_rules

    def run_uninitialized(self):
        q = list(self.uninitialized_rules)
        failed = []
        while q:
            rule = q.pop()
            try:
                assert not rule.initialized
                emits = []
                def _emit(*args):
                    emits.append(args)

                # clear error, if any
                if rule in self.error:
                    del self.error[rule]

                rule.init(emit=_emit)

            except (ZeroDivisionError, TypeError, KeyboardInterrupt, RuntimeError, OverflowError) as e:
                # TODO: should put stuff in error table, like everything else.
                e.exception_frame = rule_error_context()
                e.traceback = traceback.format_exc()

                self.error[rule] = e

                failed.append(rule)
            else:
                rule.initialized = True
                # process emits
                for e in emits:
                    self.emit(*e, delete=False)
        self.uninitialized_rules = failed

    #___________________________________________________________________________
    # Adding/removing rules

    def add_rule(self, index, init=None, query=None, head_fn=None, anf=None):

        assert index not in self.rules

        span = hide_ugly_filename(parse_attrs(init or query)['Span'])
        dyna_src = strip_comments(parse_attrs(init or query)['rule'])

        rule = self.rules[index] = Rule(index)

        rule.span = span
        rule.src = dyna_src
        rule.anf = anf
        rule.head_fn = head_fn

        if init:
            rule.init = init
            init.rule = rule
            self.uninitialized_rules.append(rule)

        elif query:
            self._gbc[head_fn].append(query)
            rule.query = query
            query.rule = rule

        else:
            assert False, "Can't add rule with out an initializer or query handler."

        # XXX: all rules should eventually have ANF tacked on, but until then...
        if anf is not None:
            agg, head, evals, unifs, result = anf[2:]
            args = (index,
                    dyna_src,
                    todyna([head, agg, result, evals, unifs]),
                    init,
                    query)

            fn = '$rule/%s' % len(args)
            if self.agg_name[fn] is None:
                self.new_fn(fn, ':=')

            rule.item = self.build(fn, *args)
            self.emit(rule.item, true, ruleix=None, variables=None, delete=False)

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
                self.uninitialized_rules.remove(rule)

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

        return self.run_agenda()

    def new_updater(self, fn, ruleix, handler):
        self.updaters[fn].append(handler)
        rule = self.rules[ruleix]
        rule.updaters.append(handler)
        handler.rule = rule

    #___________________________________________________________________________
    # Communication with Dyna compiler

    def dynac(self, filename):
        """
        Compile a file full of dyna code. Note: this routine does not pass along
        parser_state.
        """
        filename = path(filename)
        self.files.append(filename)
        out = self.tmp / filename.read_hexhash('sha1') + '.plan.py'
        #out = filename + '.plan.py'
        self.files.append(out)
        dynac(filename, out)
        return out

    def dynac_code(self, code):
        "Compile a string of dyna code."
        x = sha1()
        x.update(self.parser_state)
        x.update(code)
        dyna = self.tmp / ('%s.dyna' % x.hexdigest())
        with file(dyna, 'wb') as f:
            f.write(self.parser_state)  # include parser state if any.
            f.write(code)
        return self.dynac(dyna)

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
        # We only dump the error chart if it's non empty.
        if not self.error and not self.uninitialized_rules:
            return
        print >> out
        print >> out, red % 'Errors'
        print >> out, red % '======'

        # separate errors into aggregation errors and update handler errors
        I = defaultdict(lambda: defaultdict(list))
        E = defaultdict(lambda: defaultdict(list))
        for item, x in self.error.items():
            if isinstance(item, Rule):
                continue
            (val, es) = x
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
            for rule in self.uninitialized_rules:
                e = self.error[rule]
                print >> out, 'Failed to initialize rule:'
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


def peel(fn, item):
    """
    Find item's args in the appropriate chart. Assert that idx matches
    functor/arity, `fn`. Returns the arguments of term as a tuple of intern idxs
    and constants (possibly an empty tuple).
    """
    assert isinstance(item, Term) and  item.fn == fn
    return item.args
