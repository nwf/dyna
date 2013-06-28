import re
from term import Term, Cons, Nil

try:
    from numpy import log, exp, sqrt
    from numpy.random import uniform
except ImportError:                       # XXX: should probably issue a warning.
    from math import log, exp, sqrt
    from random import random as _random
    def uniform(a=0, b=1):
        return _random() * (b - a) + a

def split(s, delim='\s+'):
    return _todynalist(re.split(delim, s))

def pycall(name, *args):
    """
    Temporary foreign function interface - call Python functions from dyna!
    """
    x = eval(name)(*args)
    if isinstance(x, list):
        return _todynalist(x)
    return x

def _todynalist(x):
    if not x:
        return Nil
    return Cons(x[0], _todynalist(x[1:]))
