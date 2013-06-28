from term import Term, Cons, Nil

try:
    from numpy import log, exp, sqrt
    from numpy.random import uniform
except ImportError:                       # XXX: should probably issue a warning.
    from math import log, exp, sqrt
    from random import random as _random
    def uniform(a=0, b=1):
        return _random() * (b - a) + a

import re
def split(s, delim='\s+'):
    return re.split(delim, s)

# used as a work around to bring arbitrary python functions into dyna
def pycall(name, *args):
    x = eval(name)(*args)
    if isinstance(x, list):
        return todynalist(x)
    return x

def todynalist(x):
    if not x:
        return Nil
    return Cons(x[0], todynalist(x[1:]))
