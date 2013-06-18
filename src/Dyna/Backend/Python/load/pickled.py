"""
Load interpreter state using python's pickle protocol. This is the output of
`save`.

TODO: Can we merge a pickled interpreter into an existing one?

"""

import cPickle


class pickled(object):

    def __init__(self, interp=None, name=None):
        self.interp = interp
        self.name = name

    def main(self, filename):
        with file(filename, 'r') as f:
            interp = cPickle.load(f)
        return interp


if __name__ == '__main__':
    from interpreter import Interpreter, foo, none
    import sys
    pickled().main(sys.argv[1]).repl()
