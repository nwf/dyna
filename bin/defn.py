"""
Misc doctests
-------------

Call indirection
================

 >>> call['*/2'](3,4)
 12

 >>> call['*/2']('a',4)   # string*int
 'aaaa'

 >>> call['+/2']('a','b')   # string+string
 'ab'

 >>> call['//2'](3,4)    # integer division
 0

 >>> call['//2'](3.0,4)
 0.75
"""

import math, operator
from collections import defaultdict, Counter
from utils import red


class Aggregator(object):
    def __init__(self, item, name):
        self.item = item
        self.name = name
    def fold(self):
        raise NotImplementedError
    def inc(self, val):
        raise NotImplementedError
    def dec(self, val):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError
    def __repr__(self):
        return 'Aggregator(%r, %r)' % (self.item, self.name)


class BAggregator(Counter, Aggregator):
    def __init__(self, item, name):
        Aggregator.__init__(self, item, name)
        Counter.__init__(self)
    def inc(self, val):
        self[val] += 1
    def dec(self, val):
        self[val] -= 1
    def fold(self):
        return self
    def fromkeys(self, *_):
        assert False, 'bah.'


class MultisetAggregator(BAggregator):
    def __init__(self, item, name, folder):
        self.folder = folder
        BAggregator.__init__(self, item, name)
    def fold(self):
        return self.folder(self)


class LastEquals(Aggregator):
    def __init__(self, item, name):
        self.list = []
        Aggregator.__init__(self, item, name)
    def inc(self, val):
        self.list.append(val)
    def dec(self, val):
        raise NotImplementedError('dec on last equal not defined.')
    def fold(self):
        return self.list[-1]


class SetEquals(Aggregator):
    def __init__(self, item, name):
        self.set = set([])
        Aggregator.__init__(self, item, name)
    def inc(self, val):
        self.set.add(val)
    def dec(self, val):
        self.set.pop(val)
    def fold(self):
        return self.set
    def clear(self):
        self.set.clear()


def agg_bind(item, agg_decl):
    """
    Bind declarations (map functor->string) to table (storing values) and
    aggregator definition (the fold funciton, which gets executed).
    """

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

    (fn, _) = item

    if agg_decl[fn] == ':=':
        return LastEquals(item, agg_decl[fn])

    elif agg_decl[fn] == 'bag=':
        return BAggregator(item, agg_decl[fn])

    elif agg_decl[fn] == 'set=':
        return SetEquals(item, agg_decl[fn])

    else:
        return MultisetAggregator(item, agg_decl[fn], defs[agg_decl[fn]])
