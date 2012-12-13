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

# Call indirection tables defines mathematical operators and the like.
call = {'*/2': operator.mul,
        '//2': operator.div,
        '-/2': operator.sub,
        '+/2': operator.add,

        '-/1': operator.neg,

        '~/1': lambda x: not x,
        '|/1': lambda x,y: x or y,
        '&/2': lambda x,y: x and y,

        # comparisons
        '</2': operator.lt,
        '<=/2': operator.le,
        '>/2': operator.gt,
        '>=/2': operator.ge,
        '!=/2': operator.ne,
        '==/2': operator.eq,

#        '<</2': operator.lshift,
#        '>>/2': operator.rshift,

        'mod/1': operator.mod,
        'abs/1': operator.abs,
        'log': math.log,
        'exp': math.exp,

        '**/2': math.pow,
        '^/2': math.pow}   # differs from python


def agg_bind(agg_decl, table):
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
        s = [k*m for k, m in table[item].iteritems()]
        if len(s):
            return reduce(operator.add, s)

    def times_equals(item):
        s = [k**m for k, m in table[item].iteritems()]
        if len(s):
            return reduce(operator.mul, s)

    def and_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return reduce(operator.and_, s)

    def or_equals(item):
        s = [k for k, m in table[item].iteritems() if m > 0]
        if len(s):
            return reduce(operator.or_, s)

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
    agg = {}
    for fn in agg_decl:
        agg[fn] = agg_defs[agg_decl[fn]]

    return agg
