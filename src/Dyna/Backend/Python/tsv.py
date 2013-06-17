import re

interp = None
name = None

# TODO: option for stripping comments
def main(filename, ncols=None, delim='\t'):

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
