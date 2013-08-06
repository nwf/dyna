#!/usr/bin/env python
"""
Generates a visual representation of a Dyna program's rules after the
normalization process.
"""

import re, os, shutil, webbrowser
from collections import defaultdict
from utils import read_anf, path
from config import dynahome
from warnings import warn

try:
    from pygments import highlight
    from pygments.lexers import get_lexer_by_name
    from pygments.formatters import HtmlFormatter

except ImportError:
    def format_code(code):
        warn('pygments not installed.')
        return code, 0

else:
    def format_code(code):
        lexer = get_lexer_by_name("haskell")
        formatter = HtmlFormatter(linenos=False)
        c = re.sub('%', '--', code)
        pretty = highlight(c, lexer, formatter)
        # Pygments seems to toss out blank lines on top...
        offset = 0
        for x in code.split('\n'):
            if x.strip():  # stop of first non empty line
                break
            offset += 1
        pretty = re.sub('--', '%', pretty)
        return pretty, offset


cssfile = dynahome / 'src' / 'Dyna' / 'Backend' / 'Python' / 'debug-pygments.css'
jsfile = dynahome / 'external' / 'prototype-1.6.0.3.js'

class Edge(object):
    def __init__(self, head, label, body):
        self.head = head
        self.label = label
        self.body = body
    def __repr__(self):
        return '%s = %s(%s)' % (self.head, self.label, ', '.join(self.body)) if self.body else self.label


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

                # connect body variables to edge crux
                for b in e.body:
#                    print >> f, '"%s" -> "%s" [arrowhead=none];' % (b, id(e))
                    print >> f, '"%s" -> "%s";' % (b, id(e))
#                    print >> f, '"%s" -> "%s";' % (b, id(e))

                # connect head variables to edge
#                print >> f, '  "%s" [label="",shape=point];' % (id(e))
                print >> f, '  "%s" -> "%s";' % (id(e), e.head)

            # node styles
            for x in self.nodes:
                print >> f, '"%s" [%s]' % (x, ','.join('%s="%s"' % (k,v) for k,v in sty[x].items()))

            # edge styles
            for e in self.edges:
                print >> f, '"%s" [%s]' % (id(e), ','.join('%s="%s"' % (k,v) for k,v in sty[e].items()))

            print >> f, '}'

        # run graphviz to produce image
        assert 0 == os.system('(dot -Tsvg %s > %s)' % (dot, svg)), 'graphviz failed.'

        with file(svg) as f:
            return f.read()

    def show(self, name='/tmp/tmp'):
        self.render(name)
        os.system('gnome-open %s.svg 2>/dev/null' % name)

#    def get_function(self, x):
#        """
#        String of symbolic representation of ``x``, a variable or function, in
#        this expresion graph.
#        """
#
#        if isinstance(x, Edge):
#            label = re.sub('@\d+$', '', x.label)
#            if not x.body:  # arity 0
#                return label
#
#            if not label[0].isalpha() and len(x.body) == 2:
#                [a,b] = map(self.get_function, x.body)
#                return '(%s %s %s)' % (a, label, b)
#
#            return '%s(%s)' % (label, ', '.join(map(self.get_function, x.body)))
#        else:
#            if not self.incoming[x]:  # input variable
#                return x
#            [e] = self.incoming[x]
#            return self.get_function(e)

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


def isvar(x):
    return isinstance(x,str) and (x.isupper() or x.startswith('_$'))


def circuit(anf):

    g = Hypergraph()

    (g.source_lines, g.ruleix, g.aggregator, head, evals, unifs, result) = anf

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
        sty[e].update({'shape': 'rectangle'})
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

    dynafile = path(dynafile)

#    if not os.path.exists(cssfile) or not os.path.exists(jsfile):
#        print 'Debug must be run from the root of the Dyna source tree.'
#       return

    d = dynafile + '.d'
    d.mkdir_p()

    # XXX: this is sort of silly
    #shutil.copyfile(cssfile, d + '/debug-pygments.css')
    #shutil.copyfile(jsfile, d + '/prototype.js')

    with file(d / 'index.html', 'wb') as html:

        print >> html, HEADING

        print >> html, '<div id="dyna-source">'
        print >> html, '  <pre>'
        with file(dynafile) as f:
            original_code = f.read()

        pretty_code, offset = format_code(original_code)

        for lineno, line in enumerate(pretty_code.split('\n'), start=0):
            print >> html, '<a onclick="selectline(%s)">%s    </a>' % (lineno + offset, line)

        print >> html, '  </pre>'
        print >> html, '</div>'

        print >> html, '<div id="circuit-pane" style=""></div>'
        print >> html, '<div id="dopamine-pane" style=""></div>'
        print >> html, '<div id="update-handler-pane" style=""></div>'

        from dynac import dynac
        dynac(dynafile,
              out = d / 'plan',
              anf = d / 'anf',
              compiler_args = ['--dump-dopini=' + d / 'dopini',
                               '--dump-dopupd=' + d / 'dopupd'])

        print >> html, '<div style="display:none;">'

        with file(d / 'anf') as f:

            rules = [circuit(x) for x in read_anf(f.read())]

            # output map from source lines to rule index
            print >> html, '<script type="text/javascript" language="javascript">source_to_ruleix = {'
            for r in rules:
                [(_filename, bl, el)] = re.findall(r'(.*):(\d+):\d+-\1:(\d+):\d+', r.source_lines)
                for line in xrange(int(bl)-1, int(el)):
                    print >> html, '  %s: %s,' % (line, r.ruleix)   # these lines go to this rule.
            print >> html, '}; </script>'

            for g in rules:
                sty = graph_styles(g)
                svg = g.render(dynafile + '.d/rule-%s' % g.ruleix, sty)
                print >> html, '<div class="circuit-%s">%s</div>' % (g.ruleix, svg)

        # find "update plans" -- every term (edge) in a rule must have code to
        # handle an update to it's value.

        # -------------
        # Dopamine code
        with file(d + '/dopupd') as f:
            code = f.read()
            print >> html, '<h2>Update plans</h2>'
            for (ruleix, x, block) in re.findall(';; .*? ruleix=(\d+) (.*)\n([\w\W]+?)(?=;;)', code):
                print >> html, '<div class="dopamine-%s"><h3>Update %s</h3><pre>%s</pre></div>' % (ruleix, x, block.strip())

        with file(d + '/dopini') as f:
            code = f.read()
            print >> html, '<h2>Initialization plans</h2>'
            for (ruleix, block) in re.findall(';; .*? ruleix=(\d+) .*\n([\w\W]+?)(?=;;)', code):
                print >> html, '<div class="dopamine-%s"><h3>Initializer</h3><pre>%s</pre></div>' % (ruleix, block.strip())

        # ----------------
        # Python code
        with file(d + '/plan') as f:
            code = f.read()
            print >> html, '<h2>Update code</h2>'
            for block in re.split('\n\s*\n', code):
                x = re.findall('RuleIx: (\d+)\n', block)
                if not x:
                    continue
                [ruleix] = x
                lexer = get_lexer_by_name("python", stripall=True)
                formatter = HtmlFormatter(linenos=False)
                print >> html, """<div class="handler-%s"><pre>%s</pre></div>""" % (ruleix, highlight(block, lexer, formatter))

        print >> html, '</pre>'
        print >> html, '</div>'

    if browser:
        webbrowser.open(html.name)



HEADING = """
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

h2 { margin-top: 40px; }
a { cursor: pointer; }
svg { width: 700px; }
body { background: black; color: white; }

</style>

<link rel="stylesheet" href="%s">

<script type="text/javascript" language="javascript" src="%s"></script>

<script type="text/javascript" language="javascript">
function selectline(lineno) {
  var r = source_to_ruleix[lineno];
  $("update-handler-pane").innerHTML = "";
  $$(".handler-" + r).each(function (e) { $("update-handler-pane").innerHTML += e.innerHTML; });
  $("dopamine-pane").innerHTML = "";
  $$(".dopamine-" + r).each(function (e) { $("dopamine-pane").innerHTML += e.innerHTML; });
  $("circuit-pane").innerHTML = "";
  $$(".circuit-" + r).each(function (e) { $("circuit-pane").innerHTML += e.innerHTML; });
}
</script>

</head>
""" % ((dynahome / 'src/Dyna/Backend/Python/external/debug-pygments.css').abspath(),
       (dynahome / 'src/Dyna/Backend/Python/external/prototype-1.6.0.3.js').abspath())


if __name__ == '__main__':

    from argparse import ArgumentParser
    p = ArgumentParser(description=__doc__)
    p.add_argument('input', help='Path to Dyna source file.')
    p.add_argument('-x', dest='browser', action='store_false')

    argv = p.parse_args()
    main(argv.input, browser=argv.browser)
