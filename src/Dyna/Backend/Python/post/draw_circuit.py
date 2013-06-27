# -*- coding: utf-8 -*-
"""
TODO: nwf remove comments from rule source
"""

import webbrowser
from debug import Hypergraph
from cStringIO import StringIO
from utils import lexer, subst

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
        r.init(emit=_emit)

    return edges


class draw_circuit(object):
    """
    Crude visualization of circuit pertaining to state of the interpreter.
    """

    def __init__(self, interp):
        self.interp = interp

    def main(self, outfile):
        global interp
        interp = self.interp

        es = infer_edges(interp)
        c = circuit(es)

        with file(outfile, 'wb') as f:
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
