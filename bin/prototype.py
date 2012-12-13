#!/usr/bin/env python
"""
Generates a visual representation of a Dyna program rules after the
normalization process.
"""

import re, os
from collections import defaultdict, namedtuple
from utils import magenta, red, green, yellow, white, toANF, read_anf

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter


Edge = namedtuple('Edge', 'head label body')  # "body" is sometimes called the "tail"

def edge_code(x):
    return '%s = %s(%s)' % (x.head, x.label, ', '.join(x.body)) if x.body else x.label
Edge.__repr__ = edge_code


class Hypergraph(object):

    def __init__(self):
        self.incoming = {}   # "backward star" (BS)
        self.outgoing = {}   # "forward star" (FS)
        self.edges = []
        self.nodes = set()

    def edge(self, head, label, body):

        label = re.sub('"', r'\\"', label)
        head = re.sub('"', r'\\"', head)
        body = map(lambda b: re.sub('"', r'\\"', b), body)


        e = Edge(head, label, tuple(body))
        self.edges.append(e)

        # make slots in indexes for new nodes incident edge.
        if e.head not in self.nodes:
            self.incoming[e.head] = []
            self.outgoing[e.head] = []
            self.nodes.add(e.head)
        for b in e.body:
            if b not in self.nodes:
                self.outgoing[b] = []
                self.incoming[b] = []
                self.nodes.add(b)

        # update indices
        self.incoming[e.head].append(e)
        for b in e.body:
            self.outgoing[b].append(e)

        return e

    def render(self, name, sty=None):
        sty = sty or defaultdict(dict)

        dot = '%s.dot' % name
        svg = '%s.svg' % name

        # write the dot file so that we can pass it to graphviz
        with file(dot, 'wb') as f:
            print >> f, 'digraph rule {'
            print >> f, 'rankdir=LR;'  # left-to-right layout


            print >> f, 'node [style=filled,fillcolor=white];'

            print >> f, 'bgcolor="transparent";'

            print >> f, 'edge [color=white];'

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
                sty[x].update({'shape': 'circle'})
                print >> f, '"%s" [%s]' % (x, ','.join('%s="%s"' % (k,v) for k,v in sty[x].items()))

            # edge styles
            for e in self.edges:
                sty[e].update({'shape': 'rectangle'})
                print >> f, '"%s" [%s]' % (id(e), ','.join('%s="%s"' % (k,v) for k,v in sty[e].items()))

            print >> f, '}'

        print 'wrote', dot, 'compiling...'

        # run graphviz to produce image
        assert 0 == os.system('(dot -Tsvg %s > %s)' % (dot, svg)), 'graphviz failed.'

        print 'created', svg

        with file(svg) as f:
            return f.read()

    def show(self, name='/tmp/tmp'):
        self.render(name)
        os.system('gnome-open %s.svg 2>/dev/null' % name)

    def get_function(self, x):
        """
        String of symbolic representation of ``x``, a variable or function, in
        this expresion graph.
        """
        if isinstance(x, Edge):
            if not x.body:  # arity 0
                return x.label
            return '%s(%s)' % (x.label, ', '.join(map(self.get_function, x.body)))
        else:
            if not self.incoming[x]:  # input variable
                return x
            [e] = self.incoming[x]
            return self.get_function(e)

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


def show_slice(e, M):
    return [(b if bind else ':') for b, bind in zip(e.body, M)]


def isvar(x):
    return isinstance(x,str) and (x.isupper() or x.startswith('_$'))


def circuit(anf):

    (agg, head, side, evals, unifs, result) = anf

    g = Hypergraph()
    for var, op, args in evals:
        #if not args: args, op = [op], 'const'    # todo: useless special case?
        g.edge(head=var, label=op, body=args)

    for var, op, args in unifs:
#        g.edge(head=var, label='& %s(%s)' % (op, ','.join(map(str, args))), body=args)
        g.edge(head=var, label='& %s' % op, body=args)

    g.head = head
    g.result = result
    g.side = side

    g.inputs = [x for x in g.nodes if not g.incoming[x]]
    g.outputs = [x for x in g.nodes if not g.outgoing[x]]
    g.intermediate = [x for x in g.nodes if g.incoming[x] and g.outgoing[x]]

    return g


def graph_styles(g):

    sty = defaultdict(dict)   # style overrides and additions

    # edge styles
    for e in g.edges:
        if e.label.startswith('&'):  # distiguish unif edges
            sty[e].update({'style': 'filled', 'fillcolor': 'grey'})

    # node styles
    for x in g.nodes:

        if not g.incoming[x]:  # inputs (constants and variables) are yellow
            sty[x].update({'style': 'filled', 'fillcolor': 'yellow'})

            if isvar(x):   # input variables are bold
                sty[x].update({'penwidth': '3'})

        if x in g.side:
            sty[x].update({'style': 'filled', 'fillcolor': 'olivedrab2'})

    # distinguish circuit head and result
    sty[g.head].update({'style': 'filled', 'fillcolor': 'lightblue'})
    sty[g.result].update({'style': 'filled', 'fillcolor': 'salmon'})

    return sty


def main(dynafile):

    with file(dynafile) as f:
        code = f.read()

    d = dynafile + '.d'
    os.system('mkdir -p %s' % d)

    with file(d + '/index.html', 'wb') as html:

        print >> html, """\
<head>
<style>

html, body {margin:0; padding:0;}

#dyna-source { position:absolute; height: 95%; width: 42%; top: 10px; left: 0%;  }
#circuit-pane { position:absolute; width: 50%; top: 10px; left: 42%; padding-left: 5%; }
#update-handler-pane { position: absolute; top: 10px; left: 100%; width: 45%; padding-right: 5%; }

#dyna-source, #circuit-pane {
  border-right: 1px solid #666
}

h2 { margin-top: 40px; }
a { cursor: pointer; }
svg { width: 90%; height: 90%; }
body { background: black; color: white; }

</style>

<link rel="stylesheet" href="../../style.css">

<script type="text/javascript" language="javascript" src="../../bin/prototype.js"></script>

<script type="text/javascript" language="javascript">

function selectline(lineno) {
  $("update-handler-pane").innerHTML = "";
  $$(".handler-" + lineno).each(function (e) { $("update-handler-pane").innerHTML += e.innerHTML; });

  $("circuit-pane").innerHTML = "";
  $$(".circuit-" + lineno).each(function (e) { $("circuit-pane").innerHTML += e.innerHTML; });

}

</script>

</head>
"""

#        print >> html, '<h1>%s</h1>' % dynafile
#        print >> html, '<h2>Dyna source</h2>'

        print >> html, '<div id="dyna-source">'
        print >> html, '  <pre>'
        with file(dynafile) as f:

            lexer = get_lexer_by_name("haskell", stripall=True)
            formatter = HtmlFormatter(linenos=False)
            c = re.sub('%', '--', f.read())
            pretty_code = highlight(c, lexer, formatter)
            pretty_code = re.sub('--', '%', pretty_code)

            for lineno, line in enumerate(pretty_code.split('\n'), start=1):
                print >> html, '<a onclick="selectline(%s)">%s</a>' % (lineno, line.rstrip())

        print >> html, '  </pre>'
        print >> html, '</div>'

        print >> html, '<div id="circuit-pane" style=""></div>'
        print >> html, '<div id="update-handler-pane" style=""></div>'

        anf = toANF(code)

        print >> html, '<div style="display:none;">'

        print >> html, '<h2>ANF</h2>'
        print >> html, '<pre>\n%s\n</pre>' % anf.strip()

        print >> html, '<h2>Hyperedge templates</h2>'

        linenos = re.findall(';; (.*?):(\d+):\d+-.*?:(\d+):\d+', anf)

        rules = [circuit(x) for x in read_anf(anf)]

        assert len(rules) == len(linenos), 'missing line number in ANF.'

        for (i, ((_, lineno, _), g)) in enumerate(zip(linenos, rules)):
            sty = graph_styles(g)
            svg = g.render(dynafile + '.d/rule-%s' % i, sty)
            print >> html, '<div class="circuit-%s">%s</div>' % (lineno, svg)

        # find "update plans" -- every term (edge) in a rule must have code to
        # handle an update to it's value.

        print >> html, '<h2>Update plans</h2>'

        cmd = """ghc -isrc Dyna.Backend.Python -e 'processFile "%s"' """ % dynafile
        assert 0 == os.system(cmd), 'command failed:\n\t' + cmd

#        print >> html, '<pre>'

        with file(dynafile + '.plan') as f:
            code = f.read()
            print >> html, code

            for block in re.split('# --\n', code)[1:]:  # drop the begining bit.
                [(f, bline, bcol, eline, ecol, code)] = \
                    re.findall('# (.*?):(\d+):(\d+)-.*?:(\d+):(\d+)\n#.*\n([\w\W]*)',
                               block)

                code = re.compile('^\s*#.*', re.M).sub('', code.strip())

                lexer = get_lexer_by_name("python", stripall=True)
                formatter = HtmlFormatter(linenos=False)
                pretty_code = highlight(code, lexer, formatter)

#
                print >> html, """\
<div class="handler-%s">
<pre>
%s
</pre>
</div>
""" % (bline, pretty_code)

#                print magenta % '%s:%s' % (bline, eline)
#                print yellow % code

#            from debug import ip; ip()

            # connect code lines with update; anf; and rendered circuit
            # examples/papa2.dyna:37:1 - examples/papa2.dyna:37:24 :

        print >> html, '</pre>'

        print >> html, '</div>'


    print
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
