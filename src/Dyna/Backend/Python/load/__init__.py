from sexpr import sexpr
from tsv import tsv
from matrix import matrix
from pickled import pickled

import re as _re

def run(interp, line):
    try:
        [(name, module, args)] = _re.findall('^([a-z][a-zA-Z_0-9]*) = ([a-z][a-zA-Z_0-9]*)\((.*)\)', line)
    except ValueError:
        print 'Error: failed to parse post command.'
        print '    %s' % line
        print
        return

    try:
        m = getattr(__import__('load'), module)(interp, name)
    except KeyError:
        print 'did not recognize post-processor %r' % name
        return

    exec 'm.main(%s)' % args
    interp.go()
