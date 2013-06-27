# -*- coding: utf-8 -*-

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

#        webbrowser.open(f.name)

        # group edges by head then ruleindex
        global groups
        groups = groupby(lambda x: x[0], es)
        for a in groups:
            groups[a] = groupby(lambda x: x[1], groups[a])

        for head in groups:
            dig(head, set())


from collections import defaultdict
def groupby(key, data):
    g = defaultdict(list)
    for x in data:
        g[key(x)].append(x)
    return dict(g)


groups = None



def dig(head, visited, indent='', first=False, last=False):

    if last and first:
        xxx = '└─ '
    elif last:
        xxx = '└─ '
    elif first:
        xxx = '├─ '
    else:
        xxx = '├─ '

    if head in visited:
        print indent[:-4] + xxx + red % '*CYCLE*'
        return

    if head not in groups:
        return
    visited.add(head)

    print indent[:-4] + xxx + '%s = %s' % (yellow % head, _repr(head.value))

    if last:
        indent = indent[:-4] + '     '
    else:
        indent = indent + '   '

    for ruleix in groups[head]:

        contrib = groups[head][ruleix]
        for (i, (_, _, body, vs)) in enumerate(contrib):

            rule = interp.rules[ruleix]

            # TODO: nwf remove comments from rule source
            crux = Crux(head, rule, body, dict(vs))

            for line in crux.format():
                print indent + line

            print indent

            for i, x in enumerate(body):
                dig(x, visited, indent=indent + '  │ ', first=i==0, last=i==len(body)-1)

            print indent[:-2]


"""
def branch(*xs):

    for i, x in enumerate(xs):
        first = i == 0
        last = i == len(contrib)-1
        if last and first:
            xxx = '└─ '
        elif last:
            xxx = '└─ '
        elif first:
            xxx = '├─ '
        else:
            xxx = '├─ '
"""



class Crux(object):

    def __init__(self, head, rule, body, vs):
        self.head = head
        self.rule = rule
        self.body = body
        self.vs = vs
        self.graph = debug.circuit(rule.anf)

    def values(self, x):
        if x in self.vs:
            return _repr(self.vs[x])
        try:
            return _repr(eval(x.replace('\\"', '"')))
        except (SyntaxError, NameError):
            return x

    def format(self):
        rule = self.rule
        src = rule.src.replace('\n',' ').strip()
        graph = self.graph
        user_vars = dict(defn.user_vars(self.vs.items()))
        side = ['  side:   ' + self.get_function(x) for x in graph.outputs if x != rule.anf.result and x != rule.anf.head]
        return [('%s %s' % (red % rule.anf.agg, self.values(rule.anf.result))),
                (green % ('  # %s' % src)),
                (green % ('  # %s' % subst(src, user_vars))),
                (green % ('  # %s' % drepr(user_vars))),
                ('  head:   %s' % self.get_function(rule.anf.head)),
                ('  result: %s' % self.get_function(rule.anf.result))] \
                + side

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
            if not label.isalpha() and not label.startswith('& ') and len(fn_args) == 2:  # infix
                [a,b] = fn_args
                return '(%s %s %s)' % (a, label, b)
            return '%s(%s)' % (label, ', '.join(fn_args))
        else:
            if not g.incoming[x]:  # input variable
                return self.values(x)
            if len(g.incoming[x]) > 1:
                return 'UNIF ' + ' === '.join(self.get_function(e) + (red % '=%s' % self.values(e.head)) for e in g.incoming[x])
            [e] = g.incoming[x]

            if e.label == '=':
                return self.get_function(e)

            if e.label.startswith('&'):
                return self.get_function(e)

            return self.get_function(e) + (red % '=%s' % self.values(x))


import re
from utils import yellow, green, red
from defn import drepr
from term import _repr
import debug, defn
