
import cPickle

class save(object):
    """
    Save interpreter state using python's pickle protocol.
    """

    def __init__(self, interp):
        self.interp = interp

    def main(self, filename):
        with file(filename, 'wb') as f:
            cPickle.dump(self.interp, f)
