import re

class tsv(object):

    """
    Load tab-delimited files.

    TODO: option for stripping comments
    TODO: option for strict number of columns.
    """

    def __init__(self, interp, name):
        self.interp = interp
        self.name = name

    def main(self, filename, delim='\t'):

        interp = self.interp
        name = self.name

        def term(a):
            fn = '%s/%s' % (name, len(a))

            if interp.agg_name[fn] is None:
                interp.new_fn(fn, ':=')

            interp.emit(interp.build(fn, *a),
                        True,
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
