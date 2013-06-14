"""
Load interpreter state using python's pickle protocol.
"""

import cPickle

from interpreter import Interpreter, foo, none

def main():
    #out = cPickle.dumps(interp)  # XXX:
    #interp2 = cPickle.loads(out)  # XXX:
    #interp2.repl()
    with file('save.pkl', 'r') as f:
        interp = cPickle.load(f)

    interp.repl()

if __name__ == '__main__':
    main()
