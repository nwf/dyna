import re as _re
from utils import get_module

available = 'trace', 'dump_solution', 'draw_circuit', 'graph', 'save', 'draw'

def run(interp, line):

    try:
        [(module, args)] = _re.findall('([a-z][a-zA-Z_0-9]*)\((.*)\)$', line.strip())
    except ValueError:
        print 'Error: failed to parse post command.'
        print '    %s' % line
        print
        return

    if module not in available:
        print 'did not recognize post-processor %r' % module
        return

    m = get_module('post', module)(interp)
    eval('m.main(%s)' % args)
