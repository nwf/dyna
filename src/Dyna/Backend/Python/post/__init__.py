import re as _re
from save import save
from graph import graph
from draw_circuit import draw_circuit
from dump_solution import dump_solution
from trace import trace

def run(interp, line):

    try:
        [(name, args)] = _re.findall('([a-z][a-zA-Z_0-9]*)\((.*)\)$', line.strip())
    except ValueError:
        print 'Error: failed to parse post command.'
        print '    %s' % line
        print
        return

    try:
        m = globals()[name](interp)
    except KeyError:
        print 'did not recognize post-processor %r' % name
        return

    eval('m.main(%s)' % args)
