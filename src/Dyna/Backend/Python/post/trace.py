# -*- coding: utf-8 -*-
"""
Examine solution as an outline of computation.

TODO: shared substructure.

"""

import re
from utils import yellow, green, red, _repr, drepr
import debug, defn
from cStringIO import StringIO
from utils import lexer, subst

from draw_circuit import infer_edges
from collections import defaultdict


class trace(object):
    """
    Examine solution as an outline of computation.
    """

    def __init__(self, interp):
        self.interp = interp

    def main(self):
        tracer = Tracer(self.interp)
        for item in tracer.items:
            print
            print tracer(item)


class Tracer(object):

    def __init__(self, interp):
        self.interp = interp
        self.edges = infer_edges(self.interp)
        # group edges by head then ruleindex
        groups = groupby(lambda x: x[0], self.edges)
        for a in groups:
            groups[a] = groupby(lambda x: x[1], groups[a])
        self.items = groups

    def __call__(self, item):
        if item not in self.items:
            print
            print 'no trace for', item
        print '\n'.join(dig(item, set(), self.items, self.interp))


def groupby(key, data):
    g = defaultdict(list)
    for x in data:
        g[key(x)].append(x)
    return dict(g)


def dig(head, visited, groups, interp):

    if head in visited:
        return [red % '*CYCLE*']

    if head not in groups:
        return []

    visited.add(head)

    contribs = []

    for ruleix in groups[head]:
        for (_, _, body, vs) in groups[head][ruleix]:

            crux = Crux(head, interp.rules[ruleix], body, dict(vs))
            block = branch([dig(x, visited, groups, interp) for x in body])

            if block:
                contribs.append(crux.format() + ['|'] + block)
            else:
                contribs.append(crux.format())

    return ['%s = %s' % (yellow % head, _repr(head.value))] \
        + ['|'] \
        + branch(contribs) + ['']


def branch(xs):
    ys = []
    for i, x in enumerate(xs):
        first = i == 0
        last = i == len(xs)-1
        if last and first:
            h = '└─ '
        elif last:
            h = '└─ '
        elif first:
            h = '├─ '
        else:
            h = '├─ '
        if not x:
            continue
        ys.append(h + x[0])
        indent = '│  ' if not last else '   '
        for a in x[1:]:
            ys.append(indent + a)
    return ys


class Crux(object):

    def __init__(self, head, rule, body, vs):
        self.head = head
        self.rule = rule
        self.body = body
        self.vs = vs
        self.graph = debug.circuit(self.rule.anf)

    def values(self, x):
        if x in self.vs:
            return _repr(self.vs[x])
        try:
            return _repr(eval(x.replace('\\"', '"')))
        except (SyntaxError, NameError):
            return x

    def format(self):
        rule = self.rule
        src = rule.src.replace('\n',' ').strip()
        graph = self.graph
        user_vars = dict(defn.user_vars(self.vs.items()))
        side = ['side:   ' + self.get_function(x) for x in graph.outputs if x != rule.anf.result and x != rule.anf.head]
        return [('%s %s' % (red % rule.anf.agg, self.values(rule.anf.result))),
                (green % ('# %s' % src)),
                (green % ('# %s where %s' % (subst(src, user_vars), drepr(user_vars)))),
                ('head:   %s' % self.get_function(rule.anf.head)),
                ('result: %s' % self.get_function(rule.anf.result))] \
                + side

    def get_function(self, x):
        """
        String of symbolic representation of ``x``, a variable or function, in
        this expresion graph.
        """
        g = self.graph
        if isinstance(x, debug.Edge):
            label = re.sub('@\d+$', '', x.label)
            label = re.sub('^& ', '&', label)

            if label == '=':
                [b] = x.body
                return self.get_function(b)

            if not x.body:  # arity 0
                return label

            fn_args = [self.get_function(y) for y in x.body]
            if not label.isalpha() and not label.startswith('& ') and len(fn_args) == 2:  # infix
                [a,b] = fn_args
                return '(%s %s %s)' % (a, label, b)
            return '%s(%s)' % (label, ', '.join(fn_args))

        else:
            if not g.incoming[x]:  # input variable
                return self.values(x)
            if len(g.incoming[x]) > 1:
                return 'UNIFICATION ' + ' === '.join(self.get_function(e) + (red % '=%s' % self.values(e.head)) for e in g.incoming[x])
            [e] = g.incoming[x]

            if e.label == '=':
                return self.get_function(e)

            if e.label.startswith('&'):
                return self.get_function(e)

            return self.get_function(e) + (red % '=%s' % self.values(x))
