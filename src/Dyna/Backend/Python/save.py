"""
Save interpreter state using python's pickle protocol.
"""

import cPickle


def main(interp):
    with file('save.pkl', 'wb') as f:
        cPickle.dump(interp, f)
    print 'wrote', f.name
