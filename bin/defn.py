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


def agg_bind(agg, agg_decl, table):
    """
    Bind declarations (map functor->string) to table (storing values) and
    aggregator definition (the fold funciton, which gets executed).
    """

    def max_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return max(s)

    def min_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return min(s)

    def plus_equals(item):
        s = [k*m for k, m in table[item].iteritems() if m != 0]
        if len(s):
            return reduce(operator.add, s)

    def times_equals(item):
        s = [k**m for k, m in table[item].iteritems() if m != 0]
        if len(s):
            return reduce(operator.mul, s)

    def and_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return reduce(lambda x,y: x and y, s)

    def or_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return reduce(lambda x,y: x or y, s)


    # map names to functions
    agg_defs = {
        'max=': max_equals,
        'min=': min_equals,
        '+=': plus_equals,
        '*=': times_equals,
        '&=': and_equals,
        '|=': or_equals,
        ':-': or_equals,
    }

    # commit functors to an aggregator definition to avoid unnecessary lookups.
    for fn in agg_decl:

#        if agg_decl[fn] == ':=':   # XXX: leaves previous version???
#            raise NotImplementedError("aggregator ':=' not implemented yet.")
#            continue

        if fn in agg:
            if agg[fn].__name__ != agg_defs[agg_decl[fn]].__name__:
                print red % 'conflicting aggregators. %s and %s' % (agg[fn], agg_defs[agg_decl[fn]])

        # XXX: ignores conflicts with aggregator, might lead to confusion.

        # TODO: as soon as we ':=' we probably won't want this behavior and we
        # can restor the assertion that aggregator hasn't changed.
        agg[fn] = agg_defs[agg_decl[fn]]
