"""
Load interpreter state using python's pickle protocol. This is the output of
`save`.

TODO: Can we merge a pickled interpreter into an existing one?

"""

import cPickle
from IPython.external.path import path


class pickled(object):

    def __init__(self, interp=None, name=None):
        self.interp = interp
        self.name = name

    def main(self, filename):
        filename = path(filename)
        if not filename.exists():
            print 'file `%s` does not exist.' % filename
            return
        with file(filename, 'r') as f:
            interp = cPickle.load(f)
        return interp


if __name__ == '__main__':
    from interpreter import Interpreter, foo, none
    import sys
    pickled().main(sys.argv[1]).repl()
