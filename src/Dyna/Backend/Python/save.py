"""
Save interpreter state using python's pickle protocol.
"""

import cPickle


def main(interp):
    #out = cPickle.dumps(interp)  # XXX:
    #interp2 = cPickle.loads(out)  # XXX:
    #interp2.repl()
    with file('save.pkl', 'wb') as f:
        cPickle.dump(interp, f)
