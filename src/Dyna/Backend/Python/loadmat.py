"""
loadmat - Load a text file as a (jagged) matrix.

For example

1 2 3
4 5

6 7


m(0,0) := 1. m(0,1) := 2. m(0,2) := 3
m(1,0) := 4. m(1,1) := 5.

m(3,0) := 6. m(3,1) := 7.

"""

import re

interp = None
name = None

# TODO: option for strict width
# TODO: option for stripping comments
def main(filename, astype=float, delim='\s+'):

    fn = '%s/2' % name
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
