import sys

class dump_solution(object):
    """
    Print solution
    """
    def __init__(self, interp):
        self.interp = interp

    def main(self, filename=None):
        if filename is None:
            self.interp.dump_charts(sys.stdout)
        else:
            with file(filename, 'wb') as f:
                self.interp.dump_charts(f)
