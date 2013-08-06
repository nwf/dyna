import re
from utils import get_module

available = 'trace', 'dump_solution', 'draw_circuit', 'graph', 'save', 'draw'

def run(interp, line):

    line = line.strip()

    try:
        [(module, args)] = re.findall('([a-z][a-zA-Z_0-9]*)\((.*)\)$', line)
    except ValueError:
        print 'Error: failed to parse post command.'
        print '    %s' % line
        print
        return

    if module not in available:
        print 'did not recognize post-processor %r' % module
        return

    m = get_module('post', module)(interp)

    try:
        exec 'm.main(%s)' % args
    except SyntaxError as e:
        print 'Syntax error: %s' % e
        return
