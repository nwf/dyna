#!/usr/bin/env python
from path import path
from cStringIO import StringIO
from subprocess import Popen, PIPE
from argparse import ArgumentParser

import re, sys
src = 'src/Dyna/Backend/Python'
sys.path.append(src)

if not path(src).exists():
    print >> sys.stderr, 'Tests must be run in top level directory.'
    exit(1)

from utils import red, green
from dyna_doctest import run


def end_to_end(tests):
    print 'End-to-end'
    print '=========='
    for x, z in tests:
        print x,
        sys.stdout.flush()

        y = x + '.py.out'

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
            yield [x, out + '\n' + err]


def run_doctests(files):
    print
    print 'Doctests'
    print '========'
    for x in files:
        print x,
        sys.stdout.flush()
        with file(x) as f:
            g = StringIO()
            if run(f.read(), g):
                yield [x, g.getvalue()]
                print red % 'fail'
            else:
                print green % 'pass'


def main():

    parser = ArgumentParser()
    parser.add_argument('files', nargs='*', type=path)

    args = parser.parse_args()

    failures = []

    # default set of doctests
    if not args.files:
        tests = [(re.sub('(examples/)expected/(.*).py.out', r'\1\2.dyna', z), z) \
                     for z in sorted(path('examples/expected').glob("*.py.out"))]
        failures.extend(end_to_end(tests))

        args.files = [x for x in sorted(path('test').glob("*/*.dynadoc")) \
                          if '/ptb.dynadoc' not in x \
                              and '/known-failures/' not in x]

    failures.extend(run_doctests(args.files))

    # detailed failure reports
    for f, log in failures:
        print
        print '================================================'
        print f
        print '================================================'
        print log

    # failure summary
    if failures:
        print '================================================'
        print 'FAILURES (%s)' % len(failures)
        print '================================================'
        for f, _ in failures:
            print f
        exit(1)


if __name__ == '__main__':
    main()
