from sexpr import sexpr
from tsv import tsv
from matrix import matrix
from pickled import pickled

import re

def run(interp, line):
    [(name, module, args)] = re.findall('^([a-z][a-zA-Z_0-9]*) = ([a-z][a-zA-Z_0-9]*)\((.*)\)', line)
    m = getattr(__import__('load'), module)(interp, name)
    exec 'm.main(%s)' % args
    interp.go()
