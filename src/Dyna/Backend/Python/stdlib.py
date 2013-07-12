import re
from term import Term, Cons, Nil, MapsTo
from collections import Counter
from utils import pretty, pretty_print, true, false, null, isbool
from math import log, exp, sqrt
from random import random as _random


def or_(x, y):
    if not (isbool(x) and isbool(y)):
        raise TypeError('')
    return todyna(x or y)

def and_(x, y):
    if not (isbool(x) and isbool(y)):
        raise TypeError('')
    return todyna(x and y)

def not_(x):
    if not isbool(x):
        raise TypeError(repr(x))
    if x:
        return false
    else:
        return true

def gt(x, y):
    return todyna(x > y)

def gte(x, y):
    return todyna(x >= y)

def lt(x, y):
    return todyna(x < y)

def lte(x, y):
    return todyna(x <= y)

def eq(x,y):
    """
    My work around for discrepency in bool equality True==1 and False==0.

    >>> eq(true, 1)
    false

    >>> eq(1, 1.0)
    true
    """
    return todyna(x == y)

def not_eq(x, y):
    if x != y:
        return true
    else:
        return false

_range = range
def range(*x):
    return todyna(_range(*x))


def split(s, delim='\s+'):
    return todyna(re.split(delim, s))


def crash():
    class Crasher(Exception): pass
    raise Crasher('Hey, you asked for it!')


def pycall(name, *args):
    """
    Temporary foreign function interface - call Python functions from dyna!
    """
    args = tuple(topython(x) for x in args)
    x = eval(name)(*args)
    return todyna(x)


def topython(x):

    if islist(x):
        return [topython(y) for y in x.aslist]

    elif isinstance(x, MapsTo):
        return tuple(x.args)

    return x


def todyna(x):

    if isinstance(x, (set, Counter)):
        x = list(x)
        x.sort()
        return todyna(x)

    elif x is True:
        return true

    elif x is False:
        return false

    elif isinstance(x, dict):
        return todyna([MapsTo(k,v) for k,v in x.items()])

    elif isinstance(x, (list, tuple)):
        c = Nil
        for y in reversed(x):
            c = Cons(todyna(y), c)
        return c
    else:

        return x


def get(x, i):
    return x[i]


def getkey(m, k):
    return m[k]


def setkey(m, k, v):
    m[k] = v
    return m


def islist(x):
    return isinstance(x, Cons) or x is Nil


def iter_cons(x):
    if not islist(x):
        raise TypeError("Attemping to iterate something which isn't a list. %r" % (x,))
    return x.like_chart()


def in_list(x, a):
    if not islist(a):
        raise TypeError("Attemping to iterate something which isn't a list. %r" % (a,))
    return todyna(x in a.aslist)


# should probably be done with memoized backchaining...
def read_lines(filename):
    with file(filename) as f:
        return f.readlines()

def uniform(a=0, b=1):
    return _random() * (b - a) + a
