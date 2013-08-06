import re
from utils import get_module

available = 'sexpr', 'tsv', 'matrix'

def run(interp, line):

    line = line.strip()

    try:
        [(name, module, args)] = re.findall('^([a-z][a-zA-Z_0-9]*) = ([a-z][a-zA-Z_0-9]*)\((.*)\)', line)
    except ValueError:
        print 'Error: failed to parse load command.'
        print '    %s' % line
        print
        return

    if module not in available:
        print 'did not recognize loader %r' % module
        return

    m = get_module('load', module)(interp, name)

    try:
        exec 'm.main(%s)' % args
    except SyntaxError as e:
        print 'Syntax error: %s' % e
        return
    return interp.run_agenda()
