# -*- coding: utf-8 -*-

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

    def __call__(self, item, depth_limit=-1):
        if item not in self.items:
            print
            print 'no trace for', item
        print '\n'.join(dig(head = item,
                            visited = set(),
                            tail = tuple(),
                            groups = self.items,
                            interp = self.interp,
                            depth_limit = depth_limit))


def groupby(key, data):
    g = defaultdict(list)
    for x in data:
        g[key(x)].append(x)
    return dict(g)


def dig(head, visited, tail, groups, interp, depth_limit=-1):

    if depth_limit >= 0 and len(tail) >= depth_limit:
        return [yellow % '*max depth*']

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
            block = branch([dig(x, visited, tail + (head,), groups, interp, depth_limit) for x in body])

            if block:
                contribs.append(crux.fvalue() + [''] + crux.format() + ['|'] + block)
            else:
                contribs.append(crux.fvalue() + [''] + crux.format() + [''])

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
            if x.startswith('_'):  # looks like an intermediate variable and it's not in vs
                # TODO: we could double check that this is a temp variable by
                # looking it up in anf.
                return '?'
            return x

    def fvalue(self):
        # show value and aggregator.. separate from format so we can use in
        # error handling
        return ['%s %s' % (green % self.rule.anf.agg, self.values(self.rule.anf.result))]

    def format(self):
        rule = self.rule

        graph = self.graph
        side = [self.get_function(x) for x in graph.outputs if x != rule.anf.result and x != rule.anf.head]

        explode = ('%s %s %s' % (self.get_function(rule.anf.head)[1:],  # drop quote on head
                                 green % rule.anf.agg,
                                 self.get_function(rule.anf.result)))
        if side:
            side = '    ' + ', '.join('%s %s' % ('for', x,) for x in side) + '.'
        else:
            explode += '.'
        lines = [explode]
        if side:
            lines.append(side)
        return lines

    def get_function(self, x):
        self.visited = set()
        return self._get_function(x)

    def _get_function(self, x):
        """
        String of symbolic representation of ``x``, a variable or function, in
        this expresion graph.
        """

        #if x not in ('true', 'false'):
        #    if x in self.visited:
        #        #return red % '*cycle@%s*' % x
        #        return x
        self.visited.add(x)

        g = self.graph
        if isinstance(x, debug.Edge):

            # clean up edge name
            label = re.sub('@\d+$', '', x.label)  # remove evaluation index
            label = re.sub('^& ', '&', label)     # normalize prefix quote operator

            if label == '=':
                [b] = x.body
                return self._get_function(b)

            if not x.body:  # arity 0
                return label

            fn_args = [self._get_function(y) for y in x.body]

            # infix
            if (not label[0].isalpha() and label[0] not in ('$','&') and len(fn_args) == 2) \
                    or label in ('in', 'with_key', '&with_key', '->', '&->', 'is'):

                if label in {'&with_key', '->', '&->'}:
                    label = label[1:]

                [a,b] = fn_args
                return '(%s %s %s)' % (a, label, b)
            return '%s(%s)' % (label, ', '.join(fn_args))

        else:

            if not g.incoming[x]:  # input
                if re.match('u[A-Z].*', x):                          # user variable
                    return x[1:] + (cyan % '=%s' % self.values(x))
                return self.values(x)

            # handle multiple values (can happen at unification nodes)
            if len(g.incoming[x]) > 1:
                return ' = '.join('(%s%s)' % (self._get_function(e), (cyan % '=%s' % self.values(e.head))) for e in g.incoming[x])

            [e] = g.incoming[x]

            if e.label == '=':
                return self._get_function(e)

            # handle lists
            elif e.label == '& nil':
                return '[]'

            elif e.label == '& cons':
                _e = e
                a = []
                while e.label == '& cons':
                    x, xs = e.body
                    a.append(self._get_function(x))
                    if not g.incoming[xs]:
                        a.append(self._get_function(xs))
                        break
                    [e] = g.incoming[xs]

                if e.label == '& nil':
                    return '[%s]' % ', '.join(a)
                else:
                    return self._get_function(_e)          # malformed list.

            elif e.label.startswith('&'):
                return self._get_function(e)

            else:
                return self._get_function(e) + (cyan % '=%s' % self.values(x))
