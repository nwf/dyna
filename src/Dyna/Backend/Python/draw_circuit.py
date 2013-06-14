"""
Crude visualization of circuit pertaining to state of the interpreter.
"""

import webbrowser
from debug import Hypergraph
from cStringIO import StringIO

def circuit(edges):
    # create hypergraph object
    g = Hypergraph()
    for e in edges:
        head, label, body = e
        g.edge(str(head), str(label), map(str, body))
    return g


def infer_edges(interp):
    edges = set()

    # Use rule initializers to find all active hyperedges in the current Chart.
    def _emit(item, _, ruleix, variables):
        b = variables['nodes']
        b.sort()
        b = tuple(b)
        edges.add((item, ruleix, b))

    for r in interp.rules.values():
        r.init(emit=_emit)

    return edges


def main(interp):
    es = infer_edges(interp)
    c = circuit(es)

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
        interp.dump_charts(x)

        print >> f, '<div style="position:absolute;">%s</div>' \
            % '<h1>Charts</h1>%s' \
            % '<pre style="width: 500px;">%s</pre>' \
            % x.getvalue()

        print >> f, """
        <div style="width: 800px; position:absolute; left: 550px">
        <h1>Hypergraph</h1>
        %s
        </div>
        """ % c.render('circuit')

        print >> f, '</body></html>'

    webbrowser.open(f.name)
