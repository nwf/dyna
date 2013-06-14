"""
Load interpreter state using python's pickle protocol.
"""

import cPickle

from interpreter import Interpreter, foo, none


def main():
    with file('save.pkl', 'r') as f:
        interp = cPickle.load(f)
    interp.repl()


if __name__ == '__main__':
    main()
