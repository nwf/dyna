"""
Group similar crashes into a nice colorful summary.


$ rsync -a timv-li@login.clsp.jhu.edu:/home/nwf/src/dyna2/crashlogs/. ./.

Expects data to appear in `crashlogs/` directory

"""

import re
from path import path
from collections import defaultdict

black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))
bold = '\033[1m%s\033[0m'


def data():
    # group data by exception_type / message / traceback
    d = [(x, x.lines()) for x in path('crashlogs/').glob('*.log')]
    z = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    for x, ls in d:
        y = ls[-1]
        t = y.split(':')[0]
        tb = '\n'.join(l.strip() for l in ls if '-->' in l)
        tb2 = re.sub('\033\[[\d;]+m', '', tb)   # remove colors
        z[t][y][tb].append((x, tb2, ls))
    return z


def show(z, m=3):
    """
    m: then number of files to list.
    """
    for etype in z:
        print bold % yellow % '*'*79
        print etype
        for msg in z[etype]:
            print ' ', bold % yellow % '-'*77
            print ' ', msg.strip()
            for tb, xs in z[etype][msg].items():
                print
                for x, _, _ in xs[:m]:
                    print ' ', red % bold % x
                if m is not None and len(xs) > m:
                    print bold % red % '  %s more...' % (len(xs) - m)
                print
                for l in tb.split('\n'):
                    print '   ', l
                # show the complete log for first thing
#                for l in xs[0][2]:
#                    print '   ', l.rstrip()


            print

if __name__ == '__main__':
    show(data(), m=None)
