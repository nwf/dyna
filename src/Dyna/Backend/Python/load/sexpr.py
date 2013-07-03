from cStringIO import StringIO
from utils import parse_sexpr
from stdlib import todynalist


class sexpr(object):
    """
    Read lisp-style S-Expressions from a file.

    $ echo '(a (b c) (d e)) (a b (c))' > /tmp/foo
    $ ./dyna
    > load trees = sexpr("/tmp/foo")
    > sol

    Solution
    ========
    trees/1
    =======
    trees(0) = ["a", ["b", "c"], ["d", "e"]]
    trees(1) = ["a", "b", ["c"]]

    """

    def __init__(self, interp, name):
        self.interp = interp
        self.name = name

    def main(self, filename):

        interp = self.interp
        name = self.name

        def obj(*a):
            fn = '%s/%s' % (name, len(a))
            if interp.agg_name[fn] is None:
                interp.new_fn(fn, ':=')
            return interp.build(fn, *a)

        def t(xs):
            if isinstance(xs, basestring):
                return xs
            else:
                return todynalist([t(x) for x in xs])

        contents = file(filename).read()

        for i, x in enumerate(parse_sexpr(contents)):
            interp.emit(obj(i),
                        t(x),
                        ruleix=None,
                        variables=None,
                        delete=False)


# TODO: maybe really big terms should have a pretty printer
def pretty(t, initialindent=0):
    "Pretty print tree as a tabbified s-expression."
    f = StringIO()
    out = f.write
    def pp(t, indent=initialindent, indentme=True):
        if indentme:
            out(' '*indent)
        if isinstance(t, basestring):                    # base case
            return out('"%s"' % t)
        if len(t) == 1:
            if t[0]:
                pp('"%s"' % t[0], indent, indentme)
            return
        label, children = t[0], t[1:]
        label = '"%s"' % label
        assert isinstance(label, basestring)
        out('&t(%s, ' % label)
        n = len(children)
        for i, child in enumerate(children):
            pp(child, indent + len(label) + 5, i != 0)   # first child already indented
            if i != n-1:                                 # no newline after last child
                out(',\n')
        out(')')
    pp(t)
    out('\n')
    return f.getvalue()
