import re
from term import Term, Cons, Nil
from collections import Counter

try:
    from numpy import log, exp, sqrt
    from numpy.random import uniform
except ImportError:                       # XXX: should probably issue a warning.
    from math import log, exp, sqrt
    from random import random as _random
    def uniform(a=0, b=1):
        return _random() * (b - a) + a

def split(s, delim='\s+'):
    return todynalist(re.split(delim, s))

def crash():
    class Crasher(Exception):
        pass
    raise Crasher('Hey, you asked for it!')

def pycall(name, *args):
    """
    Temporary foreign function interface - call Python functions from dyna!
    """
    args = tuple(topython(x) for x in args)
    x = eval(name)(*args)
    return todyna(x)

def topython(x):
    if isinstance(x, Cons) or x is Nil:
        return x.aslist
    return x


def todynalist(x):    # TODO: get rid of this.
    return todyna(x)

def todyna(x):
    if isinstance(x, (set, Counter)):
        x = list(x)
        x.sort()
        return todyna(x)
    elif isinstance(x, (list, tuple)):
        c = Nil
        for y in reversed(x):
            c = Cons(todyna(y), c)
        return c
    else:
        return x


def get(x, i):
    return x[i]

def iter_cons(x):
    if not (isinstance(x, Cons) or x is Nil):
        raise TypeError("Attemping to iterate something which isn't a list.")
    return x

def in_list(x, a):
    if not (isinstance(a, Cons) or a is Nil):
        raise TypeError("Attemping to iterate something which isn't a list.")
    return x in a.aslist

# should probably be done with memoized backchaining...
def read_lines(filename):
    with file(filename) as f:
        return f.readlines()
