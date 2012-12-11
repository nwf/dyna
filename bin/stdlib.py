"""
Work in progress: standard library of "stuff" the "dyna executable" will make
use of.

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
        'log': math.log,
        'exp': math.exp,
        '^/2': math.pow}


# Update handler indirection -- a true hack. Allow us to have many handlers on
# the same functor/arity

def register(functor_arity):
    """
    Decorator for registering update handlers. Used by update dispatcher.

    Note: registration is with a global/mutable table.

    For testing purposes clear current handlers
    >>> register.handlers.clear()

    >>> @register("test")
    ... def _(H, V):
    ...     return 'OK', H, V
    ...
    >>> [h] = register.handlers['test']
    >>> h("head", "value")
    ('OK', 'head', 'value')

    """

    def wrap(handler):
        register.handlers[functor_arity].append(handler)
        # you can't call these guys directly. Must go thru handler
        # indirection table
        return None

    return wrap

# NOTE: global & mutable
register.handlers = defaultdict(list)


def update_dispatcher(functor_arity, idx, value):
    """
    Passes update to relevant handlers.
    """
    print 'dispatching update', (functor_arity, idx, value)
    for handler in register.handlers[functor_arity]:
        print 'sending update to handler'
        handler(idx, value)


def emit(item, value):
    (fn, idx) = item
    row = chart[fn].data[idx]
    args = row[:-1]
    was = row[-1]

    print 'emit: %s %s%s <== %s    (current value: %s)' % (item, fn, args, value, was)
    aggregator[item][value] += 1            # timv: retract passes in -1?


def peel(fn, (fa, idx)):
    """
    Lookup `idx` in the intern table. Asserts that idx matches
    `functor_arity`. Returns arguments of term as a arity-tuple of intern idxs and
    constants.
    """
    assert fa == fn
    return chart[fn].data[idx][:-1]  # minus value


def build(fn, *args):

    idx = chart[fn].lookup(*args)

    if idx is None:
        idx = chart[fn].next_id()
        chart[fn].update(idx, args + (None,))   # don't know value yet.

    return (fn, idx)


class chart_indirect(dict):
    def __missing__(self, key):
        print 'creating chart indirect for:', key
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(ncols = arity + 1)  # add one for output variable
        return c


chart = chart_indirect()


class Chart(object):

    def __init__(self, ncols):
        self.ncols = ncols
        self.data = {}
        self._id = 0

    def __getitem__(self, item):

        assert isinstance(item, tuple) and len(item) == self.ncols, \
            'item width miss match: ncols %s, item %s' % (self.ncols, len(item))

        nonslice = [(i, val) for i, val in enumerate(item) if not isinstance(val, slice)]
        slices = [i for i, val in enumerate(item) if isinstance(val, slice)]

        for row in self.data.itervalues():                    # XXX: very inefficient
            if all(row[i] == val for i, val in nonslice):
                yield tuple(row[i] for i in slices)

    def lookup(self, *args):
        "find index for these args"
        assert len(args) == self.ncols - 1
        for idx, row in self.data.iteritems():                # XXX: very inefficient
            if row[:-1] == args:
                return idx

    def update(self, ix, args):
        "Update chart"
        self.data[ix] = args

    def insert(self, args):
        self.update(self.next_id(), args)

    def next_id(self):
        idx = self._id
        self._id += 1
        return idx




map(chart['f/2'].insert,
    [(1, 1, 1),
     (1, 1, 2),
     (1, 2, 2),
     (1, 2, ("g/1", 0xDEADBEEF)),
     ("foo", "bar", "baz")])

def papa_example():

    map(chart['rewrite/3'].insert,
        [( "S",   "S",  ".", 1),
         ( "S",  "NP", "VP", 1),
         ("NP", "Det",  "N", 1),
         ("NP",  "NP", "PP", 1),
         ("VP",   "V", "NP", 1),
         ("VP",  "VP", "PP", 1),
         ("PP",   "P", "NP", 1)])

    map(chart['rewrite/2'].insert,
        [( "NP",   "Papa", 1),
         (  "N", "caviar", 1),
         (  "N",  "spoon", 1),
         (  "V",    "ate", 1),
         (  "P",   "with", 1),
         ("Det",    "the", 1),
         ("Det",      "a", 1)])

    for i, word in enumerate("Papa ate the caviar with a spoon .".split()):
        chart['phrase/3'].insert((word, i, i + 1, 1))


papa_example()


# --
# Cost: 19.0
@register("phrase/3")
def _(_H, _V):
    (Y,I,K) = peel("phrase/3", _H)
    _f1 = _V
    for (X,_f2) in chart["rewrite/2"][:,Y,:]:
        _f3 = call["*/2"](_f1,_f2)
        _u0 = build("phrase/3",X,I,K)
        emit(_u0,_f3)


execfile('examples/cky.dyna.plan')


# Agg :: item ==> [value]
aggregator = defaultdict(Counter)

# agenda hold pending work
agenda = {}

def run_agenda():
    "the main loop"
    while agenda:
        item = agenda.pop()
        new = aggregate(item)   # compute new value for item
        propagate(item, new)


def aggregate(item):
    print 'aggregate', item
    return 0.0


def delete(item):
    pass


def propagate(item, new):

    old = chart[item]

    delete(item, old)

    chart[item] = new

    # enqueue work to the agenda
