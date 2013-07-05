# -*- coding: utf-8 -*-

"""
TODO: have ANF output which functors are infix, prefix, nullary, etc.
"""

import re
from collections import defaultdict

import debug
from draw_circuit import infer_edges
from utils import yellow, green, cyan, red, _repr


class trace(object):
    """
    Examine solution as an outline of computation. Essentially it computes
    `trace` for every term in the current solution.

    See `help trace` for more information on `trace`.
    """

    def __init__(self, interp):
        self.interp = interp

    def main(self):
        tracer = Tracer(self.interp)
        for item in tracer.items:
            print
            tracer(item)


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
        print '\n'.join(dig(item, set(), tuple(), self.items, self.interp))


def groupby(key, data):
    g = defaultdict(list)
    for x in data:
        g[key(x)].append(x)
    return dict(g)


def dig(head, visited, tail, groups, interp):

    if head in tail:
        return ['%s = %s' % (yellow % head, _repr(head.value))] \
            + ['|'] \
            + branch([[red % 'continue as before (cyclic structure, will continue forever)']]) \
            + ['']

    if head in visited:
        return ['%s = %s' % (yellow % head, _repr(head.value))] \
            + ['|'] \
            + branch([[red % 'continue as before (shared structure)']]) \
            + ['']

    if head not in groups:
        return []

    visited.add(head)

    contribs = []

    for ruleix in groups[head]:
        for (_, _, body, vs) in groups[head][ruleix]:

            crux = Crux(head, interp.rules[ruleix], body, dict(vs))
            block = branch([dig(x, visited, tail + (head,), groups, interp) for x in body])

            if block:
                contribs.append(crux.format() + ['|'] + block)
            else:
                contribs.append(crux.format() + [''])

    return ['%s = %s' % (yellow % head, cyan % _repr(head.value))] \
        + ['|'] \
        + branch(contribs)


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
        #src = rule.src.replace('\n',' ').strip()
        #user_vars = dict(user_vars(self.vs.items()))

        graph = self.graph
        side = [self.get_function(x) for x in graph.outputs if x != rule.anf.result and x != rule.anf.head]

        explode = ('%s %s %s' % (self.get_function(rule.anf.head)[1:],  # drop quote on head
                                 green % rule.anf.agg,
                                 self.get_function(rule.anf.result)))

        if side:
            side = '    ' + ', '.join('%s %s' % ('for', x,) for x in side) + '.'
            explode += ','
        else:
            explode += '.'

        lines = ['%s %s' % (green % rule.anf.agg, self.values(rule.anf.result)),
                 '',
                 explode]

        if side:
            lines.append(side)

        return lines

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

            # infix
            if (not label.isalpha() and not label.startswith('&') and len(fn_args) == 2) \
                    or label in ('in', 'and', 'or'):
                [a,b] = fn_args
                return '(%s %s %s)' % (a, label, b)
            return '%s(%s)' % (label, ', '.join(fn_args))

        else:

            if not g.incoming[x]:  # input
                if re.match('u[A-Z].*', x):                          # user variable
                    return x[1:] + (cyan % '=%s' % self.values(x))
                return self.values(x)

            if len(g.incoming[x]) > 1:
                return ' = '.join('(%s%s)' % (self.get_function(e), (cyan % '=%s' % self.values(e.head))) for e in g.incoming[x])

            [e] = g.incoming[x]

            if e.label == '=':
                return self.get_function(e)

            # handle lists
            if e.label == '& cons':
                _e = e
                a = []
                while e.label == '& cons':
                    x, xs = e.body
                    [e] = g.incoming[xs]
                    a.append(self.get_function(x))
                if e.label == '& nil':
                    return '[%s]' % ', '.join(a)
                else:
                    return self.get_function(_e)          # malformed list.

            if e.label.startswith('&'):
                return self.get_function(e)

            return self.get_function(e) + (cyan % '=%s' % self.values(x))
