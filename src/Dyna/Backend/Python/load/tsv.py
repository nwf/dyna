"""
TODO: option for stripping comments
TODO: option for strict number of columns.
"""

import re
from utils import true
from path import path


class tsv(object):
    """
    Load tab-delimited files.

    > load row = tsv("test/repl/english.gr")
    > sol
    row/4
    =====
    row(0,"0","S","NP VP")      = true.
    row(1,"1.58","ROOT","S .")  = true.
    row(2,"1.58","ROOT","S !")  = true.
    row(3,"1.58","ROOT","VP !") = true.
    row(4,"3.81","VP","V")      = true.
    row(5,"3.81","VP","V NP")   = true.
    row(6,"1.49","VP","V VP")   = true.
       ...

    """

    def __init__(self, interp, name):
        self.interp = interp
        self.name = name

    def main(self, filename, delim='\t'):
        filename = path(filename)
        if not filename.exists():
            print 'file `%s` does not exist.' % filename
            return

        interp = self.interp
        name = self.name

        def term(a):
            fn = '%s/%s' % (name, len(a))

            if interp.agg_name[fn] is None:
                interp.new_fn(fn, ':-')

            interp.emit(interp.build(fn, *a),
                        true,
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
                term([i] + line)
