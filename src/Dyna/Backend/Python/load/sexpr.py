from cStringIO import StringIO
from utils import parse_sexpr
from stdlib import todyna


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
                interp.new_fn(fn, '=')
            return interp.build(fn, *a)

        contents = file(filename).read()

        for i, x in enumerate(parse_sexpr(contents)):
            interp.emit(obj(i),
                        todyna(x),
                        ruleix=None,
                        variables=None,
                        delete=False)

