#!/usr/bin/env python
"""
Generates a visual representation of a Dyna rule graphviz after the
normalization process.

TODO: to be useful as a script we should process entire *.dyna files.

"""

import re, os, sys
from collections import defaultdict, namedtuple
from os.path import abspath

red = '\033[31m%s\033[0m'


def convert(f):
    os.system('rm -f %s.anf' % f)  # clean up any existing ANF output

    assert 0 == os.system("""ghc -isrc Dyna.Analysis.NormalizeParseSelftest -e 'normalizeFile "%s"' """ % f), \
        'failed to convert file.'

    with file('%s.anf' % f) as h:
        return h.read()


def toANF(code, f):
    with file(f, 'wb') as tmp:
        tmp.write(code)
    anf = convert(tmp.name)
    print anf
    return evalthings(parse_sexpr(anf))


# by George Sakkis (gsakkis at rutgers.edu)
# http://mail.python.org/pipermail/python-list/2005-March/312004.html
def parse_sexpr(e):
    "If multiple s-expressions expected as output, set multiple to True."
    es, stack = [], []
    for token in re.split(r'([()])|\s+', e):
        if token == '(':
            new = []
            if stack:
                stack[-1].append(new)
            else:
                es.append(new)
            stack.append(new)
        elif token == ')':
            try:
                stack.pop()
            except IndexError:
                raise ValueError("Unbalanced right parenthesis: %s" % e)
        elif token:
            try:
                stack[-1].append(token)
            except IndexError:
                raise ValueError("Unenclosed subexpression (near %s)" % token)
    return es


def evalthings(t):
    if isinstance(t, str):
        try:
            return eval(t)
        except:
            return t
    else:
        return [evalthings(x) for x in t]


Edge = namedtuple('Edge', 'head label body')


class Hypergraph(object):

    def __init__(self):
        self.incoming = defaultdict(list)
        self.outgoing = defaultdict(list)
        self.edges = []
        self.nodes = set()
        self.sty = defaultdict(dict)

    def edge(self, head, label, body):
        e = Edge(head, label, tuple(body))
        self.edges.append(e)
        self.incoming[e.head].append(e)
        self.nodes.add(e.head)
        for b in e.body:
            self.outgoing[b].append(e)
            self.nodes.add(b)
        return e

    def render(self, name):
        dot = abspath('%s.dot' % name)
        png = abspath('%s.png' % name)

        # write the dot file so that we can pass it to graphviz
        with file(dot, 'wb') as f:
            print >> f, 'digraph rule {'
            print >> f, 'rankdir=LR;'  # left-to-right layout

            for e in self.edges:

                print >> f, ('"{head}" [label="{head}"];'
                             '"{id}" [label="{label}"];').format(head=e.head,
                                                                 id=id(e),
                                                                 label=e.label)

                # connect body variables to edge
                for b in e.body:
                    print >> f, '"%s" -> "%s";' % (b, id(e))

                # connect head variables to edge
                print >> f, '"%s" -> "%s";' % (id(e), e.head)

            # node styles
            for x in self.nodes:
                self.sty[x].update({'shape': 'circle'})
                print >> f, '"%s" [%s]' % (x, ','.join('%s=%s' % (k,v) for k,v in self.sty[x].items()))

            # edge styles
            for e in self.edges:
                self.sty[e].update({'shape': 'rectangle'})
                print >> f, '"%s" [%s]' % (id(e), ','.join('%s=%s' % (k,v) for k,v in self.sty[e].items()))

            print >> f, '}'

        # run graphviz to produce image
        os.system('(dot -Tpng %s > %s)' % (dot, png))


def goo(x):
    for var, val in x:
        op, args = val[0], val[1:]
        yield var, op, args


def circuit(anf):

    [(agg, head, side, evals, unifs, result)] = anf

    side = set(side[1:])

    [_, result] = result

    g = Hypergraph()
    for var, op, args in goo(evals[1:]):
        #if not args: args, op = [op], 'const'    # todo: useless special case?
        g.edge(head=var, label=op, body=args)

    for var, op, args in goo(unifs[1:]):
        e = g.edge(head=var, label='& %s(%s)' % (op, ','.join(map(str, args))), body=args)

        # distiguish unif edges
        g.sty[e].update({'style': 'filled', 'fillcolor': 'grey'})

    # node styles
    for x in g.nodes:
        if not g.incoming[x]:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'yellow'})
        if not g.outgoing[x]:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'red'})
        if x in side:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'green'})

    # variables which are projected-out to the head
#    g.sty[head].update({'penwidth': '3'})
    g.sty[head].update({'style': 'filled', 'fillcolor': 'blue'})

    return g


def test():

    examples = {
        # min-cost path in a second-order markov model
        'markov2': 'path(pair(Y,Z),V) min= path(pair(X,Y),U) + cost(X,Y,Z,U,V).',

        # Fibonacci numbers: note `X-1` is evaluated even though `f` generally
        # doesn't require it's arguments quoted
        'fib': 'f(X) := f(X-1) + f(X-2).',

        # part of the cky program
        'cky': 'phrase(X,I,K) += phrase(Y,I,J) * phrase(Z,J,K) * rewrite(X,Y,Z) * f(X,X).',

        # example which build a lot of structure.
        'structure': 'x :- g(Y, f(X, h(Z, Y)), e(q(X, Y))).',

        # monster
       'monster': ('f(X,Y) += (g(X,"string",d) - h(X,X,Y) - c)^2 + f(Y,Z)/exp(3.0)'
                   ' whenever ?c, (d < 10), e(f(h(X)), g(X)).')

    }

    if not sys.argv[1:] or not all(x in examples for x in sys.argv[1:] if x != 'all'):
        print 'usage:\n\t%s [%s]+' % (sys.argv[0], '|'.join(examples.keys()))
        return

    os.system('rm -f /tmp/*.png')

    for name in (examples if 'all' in sys.argv[1:] else sys.argv[1:]):
        print red % name

        code = examples[name]
        print code

        g = circuit(toANF(code, '/tmp/' + name + '.dyna'))

        g.render('/tmp/' + name)

    os.system('gnome-open /tmp/*.png 2>/dev/null')


if __name__ == '__main__':
    test()
