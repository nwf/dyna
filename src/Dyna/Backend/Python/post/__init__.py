import re
from save import save
from graph import graph
from draw_circuit import draw_circuit
from dump_chart import dump_chart


def run(interp, line):
    [(name, args)] = re.findall('([a-z][a-zA-Z_0-9]*)\((.*)\)$', line)
    m = globals()[name](interp)
    eval('m.main(%s)' % args)
