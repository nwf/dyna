#!/usr/bin/env python
"""
Generates a visual representation of a Dyna program rules after the
normalization process.
"""

# TODO: add a directory of sample Dyna programs to repo (Fibonacci, CKY,
# second-order markov, shortest-path)

import re, os, sys
from collections import defaultdict, namedtuple
from pprint import pprint


black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


def convert(f):
    os.system('rm -f %s.anf' % f)  # clean up any existing ANF output

    assert 0 == os.system("""ghc -isrc Dyna.Analysis.NormalizeParseSelftest -e 'normalizeFile "%s"' """ % f), \
        'failed to convert file.'

    with file('%s.anf' % f) as h:
        return h.read()


def toANF(code, f='/tmp/tmp.dyna'):
    with file(f, 'wb') as tmp:
        tmp.write(code)
    return convert(tmp.name)


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
            return eval(t)   # FIXME: dangerous way to do this.
        except:
            return t
    else:
        return [evalthings(x) for x in t]


def read_anf(e):
    x = evalthings(parse_sexpr(e))

    def g(x):
        return [(var, val[0], val[1:]) for var, val in x]

    for (agg, head, side, evals, unifs, [_,result]) in x:
        yield (agg,
               head,
               side[1:],
               g(evals[1:]),
               g(unifs[1:]),
               result)


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
        dot = '%s.dot' % name
        png = '%s.png' % name

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

        print 'wrote', dot, 'compiling...'

        # run graphviz to produce image
        assert 0 == os.system('(dot -Tpng %s > %s)' % (dot, png)), 'graphviz failed.'

        print 'created', png

        return png

    def show(self, name='/tmp/tmp'):
        os.system('gnome-open %s 2>/dev/null' % self.render(name))

    def get_function(self, x):
        """
        String of symbolic representation of ``x``, a variable or function, in
        this expresion graph.
        """

        if isconst(x):
            return str(x)

        elif not isinstance(x, Edge):
            if x not in self.incoming or not self.incoming[x]:  # input variable
                return x
            [e] = self.incoming[x]    # only one incoming edge per variable in this type of graph
            return self.get_function(e)

        else:
            return '%s(%s)' % (x.label, ', '.join(map(self.get_function, x.body)))

    def toposort(self, root):
        visited = set()

        def t(v):
            if v not in visited:
                visited.add(v)
                for e in self.incoming[v]:
                    for u in e.body:
                        for b in t(u):  # "yield from" recursive call
                            yield b
                yield v

        return t(root)

    def plan(self, root):

        incoming = self.incoming

        [e] = incoming[root]

        q = [e]
        c = {e: 0 for e in self.edges}

        while q:
            e = q.pop()
            print e

            c[e] = 1

            for c in (c for b in e.body for c in incoming[b]):  # edge->node->edge

                # is edge traversable?

                q.append(c)

        pprint(e)


def isconst(x):
    return isinstance(x, (float, int))



def circuit(anf):

    (agg, head, side, evals, unifs, result) = anf

    g = Hypergraph()
    for var, op, args in evals:
        #if not args: args, op = [op], 'const'    # todo: useless special case?
        g.edge(head=var, label=op, body=args)

    for var, op, args in unifs:
#        e = g.edge(head=var, label='& %s(%s)' % (op, ','.join(map(str, args))), body=args)
        e = g.edge(head=var, label=op, body=args)

        # distiguish unif edges
        g.sty[e].update({'style': 'filled', 'fillcolor': 'grey'})

    # node styles
    for x in g.nodes:
        if not g.incoming[x]:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'yellow'})

            if isinstance(x,str) and (x.isupper() or x.startswith('_$')):   # variables are bold
                g.sty[x].update({'penwidth': '3'})

        if not g.outgoing[x]:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'salmon'})
        if x in side:
            g.sty[x].update({'style': 'filled', 'fillcolor': 'olivedrab2'})

    # variables which are projected-out to the head
    g.sty[head].update({'style': 'filled', 'fillcolor': 'lightblue'})

    g.head = head
    g.result = result
    g.inputs = [x for x in g.nodes if not g.incoming[x]]
    g.outputs = [x for x in g.nodes if not g.outgoing[x]]
    g.intermediate = [x for x in g.nodes if g.incoming[x] and g.outgoing[x]]
    g.side = side

    return g


def main(dynafile):

    with file(dynafile) as f:
        code = f.read()

    d = dynafile + '.d'
    os.system('mkdir -p %s' % d)

    with file(d + '/index.html', 'wb') as html:

        print >> html, '<style>'
        print >> html, '.box, pre { border: 1px solid #eee; margin: 10px; padding: 10px;}'
        print >> html, 'h2 { margin-top: 40px; }'
        print >> html, '</style>'

        print >> html, '<h1>%s</h1>' % dynafile

        print >> html, '<h2>Dyna source</h2>'
        print >> html, '<pre>\n%s\n</pre>' % code.strip()

        anf = toANF(code)

        print >> html, '<h2>ANF</h2>'
        print >> html, '<pre>\n%s\n</pre>' % anf.strip()

        print >> html, '<h2>Hyperedge templates</h2>'

        rules = []

        for i, x in enumerate(read_anf(anf)):
            g = circuit(x)

            rules.append(g)

            g.render(dynafile + '.d/rule-%s' % i)

            png = 'rule-%s.png' % i  # path relative (to html file)

            print >> html, '<div class="box"><img src="%s" /></div>' % png

    print 'wrote', html.name

    if argv.browser:
        os.system('gnome-open %s 2>/dev/null >/dev/null' % html.name)


if __name__ == '__main__':

    from argparse import ArgumentParser
    p = ArgumentParser(description=__doc__)
    p.add_argument('input', help='Path to Dyna source file.')
    p.add_argument('-x', dest='browser', action='store_false')

    argv = p.parse_args()
    main(argv.input)
