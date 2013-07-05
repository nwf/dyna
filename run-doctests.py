from path import path
from cStringIO import StringIO

import sys
sys.path.append('src/Dyna/Backend/Python')

print
print 'Doctests'
print '========'

from dyna_doctest import run
from utils import red, green

failures = []
for x in path('test/repl').glob("*.dynadoc"):
    print x,
    with file(x) as f:
        g = StringIO()
        if run(f.read(), g):
            failures.append([x, g.getvalue()])
            print red % 'fail'
        else:
            print green % 'pass'

for f, log in failures:
    print
    print '================================================'
    print f
    print '================================================'
    print log

if failures:
    exit(1)
