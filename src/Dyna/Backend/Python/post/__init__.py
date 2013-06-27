import re as _re
from save import save
from graph import graph
from draw_circuit import draw_circuit
from dump_solution import dump_solution


def run(interp, line):
    [(name, args)] = _re.findall('([a-z][a-zA-Z_0-9]*)\((.*)\)$', line.strip())
    m = globals()[name](interp)
    eval('m.main(%s)' % args)
