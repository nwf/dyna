#!/usr/bin/env python
"""
Generates a visual representation of a Dyna program rules after the
normalization process.
"""

import re, os, shutil, webbrowser
from collections import defaultdict, namedtuple
from cStringIO import StringIO
from utils import magenta, red, green, yellow, white, read_anf
from config import dynahome

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

cssfile="%s/src/Dyna/Backend/Python/debug-pygments.css" % dynahome
jsfile="%s/external/prototype-1.6.0.3.js" % dynahome

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

        # TODO: misses orphaned nodes.

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
#                sty[x].update({'shape': 'circle'})
                sty[e].update({'shape': 'rectangle'})
                print >> f, '"%s" [%s]' % (x, ','.join('%s="%s"' % (k,v) for k,v in sty[x].items()))

            # edge styles
            for e in self.edges:
                sty[e].update({'shape': 'rectangle'})
                print >> f, '"%s" [%s]' % (id(e), ','.join('%s="%s"' % (k,v) for k,v in sty[e].items()))

            print >> f, '}'

#        print 'wrote', dot, 'compiling...'

        # run graphviz to produce image
        assert 0 == os.system('(dot -Tsvg %s > %s)' % (dot, svg)), 'graphviz failed.'

#        print 'created', svg

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


#def show_slice(e, M):
#    return [(b if bind else ':') for b, bind in zip(e.body, M)]


def isvar(x):
    return isinstance(x,str) and (x.isupper() or x.startswith('_$'))


def circuit(anf):

    (agg, head, evals, unifs, result) = anf

    g = Hypergraph()
    for var, op, args in evals:
        g.edge(head=var, label=op, body=args)

    for var, op, val in unifs:
        if op == '&' :
            op = '%s %s' % (op,val[0])
            val = val[1:]
        g.edge(head=var, label='%s' % op, body=val)

    g.head = head
    g.result = result

    g.inputs = [x for x in g.nodes if not g.incoming[x]]
    g.outputs = [x for x in g.nodes if not g.outgoing[x]]
    g.intermediate = [x for x in g.nodes if g.incoming[x] and g.outgoing[x]]

    return g


def graph_styles(g):

    sty = defaultdict(dict)   # style overrides and additions

    # edge styles
    for e in g.edges:
        if    e.label.startswith('&') \
           or e.label.startswith('!') \
           or e.label.startswith('=') :  # distiguish unif edges
            sty[e].update({'style': 'filled', 'fillcolor': 'grey'})

    # node styles
    for x in g.nodes:

        if not g.incoming[x]:  # inputs (constants and variables) are yellow
            sty[x].update({'style': 'filled', 'fillcolor': 'yellow'})

            if isvar(x):   # input variables are bold
                sty[x].update({'penwidth': '3'})

    # distinguish circuit head and result
    sty[g.head].update({'style': 'filled', 'fillcolor': 'lightblue'})
    sty[g.result].update({'style': 'filled', 'fillcolor': 'salmon'})

    return sty


def main(dynafile, browser=True):

    if not os.path.exists(cssfile) or not os.path.exists(jsfile):
        print("Debug must be run from the root of the Dyna source tree")
        return

    d = dynafile + '.d'
    os.system('mkdir -p %s' % d)

    shutil.copyfile(cssfile,d+"/debug-pygments.css")
    shutil.copyfile(jsfile,d+"/prototype.js")

    with file(d + '/index.html', 'wb') as html:

        print >> html, """\
<head>
<style>

html, body {margin:0; padding:0; width: 5000px;}

#dyna-source, #circuit-pane, #dopamine-pane, #update-handler-pane {
  padding-right: 10px;
  width: 700px;
  display: inline;
  float: left;
  padding: 20px;
}

#dyna-source { width: 500px; }
#circuit-pane { width: 700px; }
#dopamine-pane { width: 500px; }
#update-handler-pane { width: 500px; }

#dyna-source, #circuit-pane, #dopamine-pane {
  border-right: 1px solid #666
}

h2 { margin-top: 40px; }
a { cursor: pointer; }
svg { width: 95%; height: 97%; }
body { background: black; color: white; }

</style>

<link rel="stylesheet" href="debug-pygments.css">

<script type="text/javascript" language="javascript" src="prototype.js"></script>

<script type="text/javascript" language="javascript">

function selectline(lineno) {
  $("update-handler-pane").innerHTML = "";
  $$(".handler-" + lineno).each(function (e) { $("update-handler-pane").innerHTML += e.innerHTML; });

  $("dopamine-pane").innerHTML = "";
  $$(".dopamine-" + lineno).each(function (e) { $("dopamine-pane").innerHTML += e.innerHTML; });

  $("circuit-pane").innerHTML = "";
  $$(".circuit-" + lineno).each(function (e) { $("circuit-pane").innerHTML += e.innerHTML; });

}

</script>

</head>
"""

        print >> html, '<div id="dyna-source">'
        print >> html, '  <pre>'

        with file(dynafile) as f:
            code = f.read().strip()

        lexer = get_lexer_by_name("haskell")
        formatter = HtmlFormatter(linenos=False)
        c = re.sub('%', '--', code)
        pretty_code = highlight(c, lexer, formatter)
        pretty_code = re.sub('--', '%', pretty_code)

        for lineno, line in enumerate(pretty_code.split('\n'), start=1):
            print >> html, '<a onclick="selectline(%s)">%s</a>' % (lineno, line.rstrip())

        print >> html, '  </pre>'
        print >> html, '</div>'

        print >> html, '<div id="circuit-pane" style=""></div>'
        print >> html, '<div id="dopamine-pane" style=""></div>'
        print >> html, '<div id="update-handler-pane" style=""></div>'

        cmd = """%s/dist/build/dyna/dyna -B python \
--dump-anf="%s"/anf \
--dump-dopini="%s"/dopini \
--dump-dopupd="%s"/dopupd \
-o "%s"/plan "%s" """ % (dynahome,d,d,d,d,dynafile)
        if 0 != os.system(cmd):
            print 'command failed:\n\t' + cmd
            os.system('gnome-open %s 2>/dev/null >/dev/null' % html.name)
            return

        print >> html, '<div style="display:none;">'

        with file(d + '/anf') as f:
            anf = f.read()

            # Suppress this since we display ANF graphically instead.
            # print >> html, '<h2>ANF</h2>'
            # print >> html, '<pre>\n%s\n</pre>' % anf.strip()

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

        with file(d + '/dopupd') as f:
            code = f.read()

            print >> html, '<h2>Update plans</h2>'

            for (f,bline,bcol,eline,ecol,kv,block) in \
                re.findall(';; (.*?):(\d+):(\d+)-.*?:(\d+):(\d+) (.*)\n((?: [^\n]*\n)*)'
                          , code) :

                # [fa] = re.findall('fa=([^ ]*)', kv)

                print >> html, """\
<div class="dopamine-%s">
<pre>
Update %s
%s
</pre>
</div>
""" % (bline, kv, block)


        with file(d + '/dopini') as f:
            code = f.read()

            print >> html, '<h2>Initialization plans</h2>'

            for (f,bline,bcol,eline,ecol,kv,block) in \
                re.findall(';; (.*?):(\d+):(\d+)-.*?:(\d+):(\d+) (.*)\n((?: [^\n]*\n)*)'
                          , code) :

                print >> html, """\
<div class="dopamine-%s">
<pre>
Initializer:
%s
</pre>
</div>
""" % (bline, block)

        with file(d + '/plan') as f:
            code = f.read()
            # print >> html, code

            print >> html, '<h2>Update code</h2>'

            for block in re.split('\n\s*\n', code):

                x = re.findall('Span:\s*(.*?):(\d+):(\d+)-.*?:(\d+):(\d+)\n',
                               block)

                if not x:
                    continue

                [(f, bline, bcol, eline, ecol)] = x
                code = block
                lexer = get_lexer_by_name("python", stripall=True)
                formatter = HtmlFormatter(linenos=False)
                pretty_code = highlight(code, lexer, formatter)

                print >> html, """\
<div class="handler-%s">
<pre>
%s
</pre>
</div>
""" % (bline, pretty_code)

        print >> html, '</pre>'
        print >> html, '</div>'

    if browser:
        webbrowser.open(html.name)


def hypergraph(interpreter):
    # collect edges
    interpreter.collect_edges()
    # create hypergraph object
    g = Hypergraph()
    for c in interpreter.chart.values():
        for x in c.intern.values():
            for e in interpreter.edges[x]:
                label, body = e
                g.edge(str(x), str(label), map(str, body))
    return g


def draw(interpreter):
    g = hypergraph(interpreter)
    with file('/tmp/state.html', 'wb') as f:
        print >> f, """
        <html>
        <head>
        <style>
        body {
          background-color: black;
          color: white;
        }
        </style>
        </head>
        <body>
        """

        x = StringIO()
        interpreter.dump_charts(x)

        print >> f, '<div style="position:absolute;">%s</div>' \
            % '<h1>Charts</h1>%s' \
            % '<pre style="width: 500px;">%s</pre>' \
            % x.getvalue()

        print >> f, """
        <div style="width: 800px; position:absolute; left: 550px">
        <h1>Hypergraph</h1>
        %s
        </div>
        """ % g.render('/tmp/hypergraph')

        print >> f, '</body></html>'

    webbrowser.open(f.name)


if __name__ == '__main__':

    from argparse import ArgumentParser
    p = ArgumentParser(description=__doc__)
    p.add_argument('input', help='Path to Dyna source file.')
    p.add_argument('-x', dest='browser', action='store_false')

    argv = p.parse_args()
    main(argv.input, browser=argv.browser)
