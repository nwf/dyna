# -*- coding: utf-8 -*-
"""
TODO: draw subgraph of nodes matching a query.
"""

import re
import webbrowser
from debug import Hypergraph
from cStringIO import StringIO
from utils import lexer, subst

HEADER = """
<html>
<head>
<style>
body {
  background-color: black;
  color: white;
}
</style>

<!--
<script type="text/javascript" language="javascript" src="http://code.jquery.com/jquery-1.10.2.min.js"></script>
<script>
function clicked(x) {
  alert(x.childNodes);
}
</script>
-->

</head><body>
"""

FOOTER = """
</body></html>
"""

def circuit(edges):
    # create hypergraph object
    g = Hypergraph()
    for e in edges:
        head, label, body, _vs = e
        g.edge(str(head), str(label), map(str, body))
    return g


def infer_edges(interp):
    edges = set()

    # Use rule initializers to find all active hyperedges in the current Chart.
    def _emit(item, _, ruleix, variables):
        b = list(dict(variables)['nodes'])
        b.sort()
        # variable values not needed to name the edge, but adding them doesn't
        # change anything.
        edges.add((item, ruleix, tuple(b), variables))

    for r in interp.rules.values():
        if r.init is not None:
            try:
                r.init(emit=_emit)
            except (TypeError, ValueError, AssertionError, ZeroDivisionError):
                pass
        else:
            assert r.query is not None

    # todo: this might pick nodes that aren't used
    for fn, hs in interp._gbc.items():
        for x in interp.chart[fn].intern.values():
            if x.value is not None:
                for h in hs:
                    h(*x.args, emit=_emit)

    return edges


class draw_circuit(object):
    """
    Crude visualization of circuit pertaining to state of the interpreter.
    """

    def __init__(self, interp):
        self.interp = interp

    def main(self, outfile, open=True):
        interp = self.interp

        es = infer_edges(interp)
        c = circuit(es)

        from collections import defaultdict
        sty = defaultdict(dict)
        for e in c.edges:
            sty[e].update(dict(shape='circle',
                               width='.125',
                               fillcolor="yellow",
                               label=''))

        with file(outfile, 'wb') as f:
            print >> f, HEADER

            x = StringIO()
            interp.dump_charts(x)

            print >> f, '<div style="position:absolute;">%s</div>' \
                % '<h1>Solution</h1>%s' \
                % '<pre style="width: 500px;">%s</pre>' \
                % x.getvalue()

            svg = c.render('circuit', sty=sty)

            E = {str(id(e)): e for e in c.edges}

            def foo(m):
                z, i, cls, x, q = m.groups()

                if cls == 'edge':
                    # split on arrow ->
                    u, v = x.split('&#45;&gt;')
                    print 'edge:', i, u, v
                else:
                    if x in E:
                        print 'crux', i, x,
                        rule = interp.rules[int(E[x].label)]
                        x = '%s   %% rule %s' % (rule.src, rule.index)
                    else:
                        print 'node:', i, x

                return '<g id="%s" class="%s"><title>%s</title>%s</g>' % (i, cls, x, q)

            svg = re.sub('<g (id="([^"]+)" class="(node|edge)"><title>([^<>]+)</title>([\w\W]+?)</g>)', foo, svg)

            print >> f, """
            <div style="width: 800px; position:absolute; left: 550px">
            <h1>Hypergraph</h1>
            %s
            </div>
            """ % svg

            print >> f, FOOTER

        if open:
            webbrowser.open(f.name)
