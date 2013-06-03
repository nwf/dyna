import math, operator
from collections import defaultdict, Counter
from utils import red


class Aggregator(object):
    def __init__(self, name):
        self.name = name
    def fold(self):
        raise NotImplementedError
    def inc(self, val, ruleix, variables):
        raise NotImplementedError
    def dec(self, val, ruleix, variables):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError
    def __repr__(self):
        return 'Aggregator(%r)' % (self.name)


class BAggregator(Counter, Aggregator):
    def __init__(self, name):
        Aggregator.__init__(self, name)
        Counter.__init__(self)
    def inc(self, val, ruleix, variables):
        self[val] += 1
    def dec(self, val, ruleix, variables):
        self[val] -= 1
    def fold(self):
        return self
    def fromkeys(self, *_):
        assert False, 'bah.'


class MultisetAggregator(BAggregator):
    def __init__(self, name, folder):
        self.folder = folder
        BAggregator.__init__(self, name)
    def fold(self):
        return self.folder(self)


class LastEquals(BAggregator):
    def inc(self, val, ruleix, variables):
        self[ruleix, val] += 1
    def dec(self, val, ruleix, variables):
        self[ruleix, val] -= 1
    def fold(self):
        return max(x for x, cnt in self.items() if cnt > 0)[1]


class SetEquals(Aggregator):
    def __init__(self, name):
        self.set = set([])
        Aggregator.__init__(self, name)
    def inc(self, val, ruleix, variables):
        self.set.add(val)
    def dec(self, val, ruleix, variables):
        self.set.remove(val)
    def fold(self):
        return self.set
    def clear(self):
        self.set.clear()




def majority_equals(a):
    [(k,_)] = a.most_common(1)
    return k

def max_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return max(s)

def min_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return min(s)

def plus_equals(a):
    s = [k*m for k, m in a.iteritems() if m != 0]
    if len(s):
        return reduce(operator.add, s)

def times_equals(a):
    s = [k**m for k, m in a.iteritems() if m != 0]
    if len(s):
        return reduce(operator.mul, s)

def and_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(lambda x,y: x and y, s)

def or_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(lambda x,y: x or y, s)

def b_and_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(operator.and_, s)

def b_or_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(operator.or_, s)

# map names to functions
defs = {
    'max=': max_equals,
    'min=': min_equals,
    '+=': plus_equals,
    '*=': times_equals,
    'and=': and_equals,
    'or=': or_equals,
    '&=': b_and_equals,
    '|=': b_or_equals,
    ':-': or_equals,
    'majority=': majority_equals,
}


def aggregator(name):
    "Create aggregator by ``name``."

    if name is None:
        return None

    if name == ':=':
        return LastEquals(name)

    elif name == 'bag=':
        return BAggregator(name)

    elif name == 'set=':
        return SetEquals(name)

    else:
        return MultisetAggregator(name, defs[name])
