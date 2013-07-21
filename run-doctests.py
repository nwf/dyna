#!/usr/bin/env python
from path import path
from cStringIO import StringIO
from subprocess import Popen, PIPE

import re, sys
src = 'src/Dyna/Backend/Python'
sys.path.append(src)

if not path(src).exists():
    print >> sys.stderr, 'Tests must be run in top level directory.'
    exit(1)

from utils import red, green
from dyna_doctest import run

failures = []

print 'End-to-end'
print '=========='

for z in sorted(path('examples/expected').glob("*.py.out")):

    x = re.sub('(examples/)expected/(.*).py.out', r'\1\2.dyna', z)
    y = x + '.py.out'

    print x,
    sys.stdout.flush()

    # run ./dyna
    p = Popen(['./dyna', x, '-o', y], stdout=PIPE, stderr=PIPE)
    (out, err) = p.communicate()

    assert not p.returncode, out + '\n' + err

    # look at diff
    p = Popen(['diff', y, z], stdout=PIPE, stderr=PIPE)
    (out, err) = p.communicate()

    if not p.returncode:
        print green % 'pass'
    else:
        print red % 'fail'
        failures.append([x, out + '\n' + err])

print
print 'Doctests'
print '========'

for x in sorted(path('test').glob("*/*.dynadoc")):

    if '/ptb.dynadoc' in x:
        continue

    if '/known-failures/' in x:
        continue

    print x,
    sys.stdout.flush()
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
    print '================================================'
    print 'FAILURES (%s)' % len(failures)
    print '================================================'
    for f, _ in failures:
        print f
    exit(1)
