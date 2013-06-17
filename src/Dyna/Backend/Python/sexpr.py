import sys
from cStringIO import StringIO
from utils import parse_sexpr


if __name__ == '__main__':

    def t(xs):
        if isinstance(xs, basestring):
#            return '"%s"' % xs
            return xs
        else:
            assert len(xs) > 1
            if len(xs) == 2:
                [sym, a] = map(t, xs)
#                return '&t(%s)' % ', '.join(t(x) for x in xs)
                return [sym, a]
            elif len(xs) == 3:
                [sym, a, b] = map(t, xs)
#                return '&t(%s, %s, %s)' % (sym, a, b)
                return [sym, a, b]
            else:
                [sym, a] = t(xs[0]), t(xs[1])
                rest = t(['@' + xs[0]] + xs[2:])
#                return '&t(%s, %s, %s)' % (sym, a, rest)
                return [sym, a, rest]


    def check_binary(x):
        if isinstance(x, basestring):
            return True
        elif len(x) in (2, 3):
            return all(map(check_binary, x))
        else:
            return False


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

    for i, [x] in enumerate(parse_sexpr(sys.stdin.read())):
        btree = t(x)

        assert check_binary(btree)
        print
        print 'sentence(%s) :=\n%s.' % (i, pretty(btree, 4).rstrip())
        print
