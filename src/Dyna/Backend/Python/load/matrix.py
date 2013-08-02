import re
from IPython.external.path import path


class matrix(object):
    """
    Load a text file as a (jagged) matrix.

    For example

    $ echo 1 2 3 >  /tmp/foo
    $ echo 4 5   >> /tmp/foo
    $ echo       >> /tmp/foo
    $ echo 6 7   >> /tmp/foo

    > load m = matrix("/tmp/foo")
    > sol

    Solution
    ========
    m/2
    ===
    m(0,0) = 1.0.
    m(0,1) = 2.0.
    m(0,2) = 3.0.
    m(1,0) = 4.0.
    m(1,1) = 5.0.
    m(3,0) = 6.0.
    m(3,1) = 7.0.


    As you can see, by default values are interpreted as floats, to specify a
    different type simple passing a function which will convert strings to the
    appropriate time (e.g. str, int, float).

    > load m = matrix("/tmp/foo", astype=str)
    > sol

    Solution
    ========
    m/2
    ===
    m(0,0) = "1".
    m(0,1) = "2".
    m(0,2) = "3".
    m(1,0) = "4".
    m(1,1) = "5".
    m(3,0) = "6".
    m(3,1) = "7".

    """

    def __init__(self, interp, name):
        self.interp = interp
        self.name = name

    # TODO: option for strict width
    # TODO: option for stripping comments
    def main(self, filename, astype=float, delim='\s+'):
        filename = path(filename)
        if not filename.exists():
            print 'file `%s` does not exist.' % filename
            return

        interp = self.interp

        fn = '%s/2' % self.name
        if interp.agg_name[fn] is None:
            interp.new_fn(fn, ':=')

        def term(a, v):
            interp.emit(interp.build(fn, *a),
                        v,
                        ruleix=None,
                        variables=None,
                        delete=False)

        with file(filename) as f:
            for i, line in enumerate(f):
                line = line.rstrip()
                if not line:
                    continue
                if delim is not None:
                    line = re.split(delim, line)
                else:
                    line = [line]
                for j, v in enumerate(line):
                    term((i, j), astype(v))
