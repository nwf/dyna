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

# TODO: fuse update handlers instead of dispatching to each one independently.

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


def update_dispatcher(fn, idx, value):
    """
    Passes update to relevant handlers.
    """
    print 'dispatching update', (fn, idx, value)
    for handler in register.handlers[fn]:
        print 'sending update to handler'
        handler(idx, value)


def emit(item, value):
    (fn, idx) = item
    row = chart[fn].data[idx]
    args = row[:-1]
    was = row[-1]

    print 'emit: %s %s%s <== %s    (current value: %s)' % (item, fn, args, value, was)
    aggregator[item][value] += 1            # timv: retract passes in -1?
    agenda.append(item)


def peel(fn, (fa, idx)):
    """
    Lookup `idx` in the intern table. Asserts that idx matches
    `functor_arity`. Returns arguments of term as a arity-tuple of intern idxs and
    constants.
    """
    assert fa == fn
    return chart[fn].data[idx][:-1]  # minus value


def build(fn, *args):

    idx = chart[fn].lookup(*args)   # lookup doesn't require value?

    if idx is None:
        idx = chart[fn].insert(args + (None,))   # don't know value yet.

    return (fn, idx)


# TODO: as soon as we have safe names for these things we can get rid of this.
class chart_indirect(dict):
    def __missing__(self, key):
        print 'creating chart indirect for:', key
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(name = key,
                              ncols = arity + 1)  # add one for output variable
        return c

chart = chart_indirect()

def dump_charts():
    print
    print 'Charts'
    print '============'
    for x in chart:
        print x
        for idx, row in chart[x].data.items():
            print '%-30s := %s' % (pretty((x,idx)), row[-1])
        print


class Chart(object):

    def __init__(self, name, ncols):
        self.name = name
        self.ncols = ncols
        self.data = {}
        self._id = 0

    def __getitem__(self, item):
        assert isinstance(item, tuple) and len(item) == self.ncols, \
            'item width mismatch: ncols %s, item %s' % (self.ncols, len(item))

        nonslice = [(i, val) for i, val in enumerate(item) if not isinstance(val, slice)]
        slices = [i for i, val in enumerate(item) if isinstance(val, slice)]

        for row in self.data.itervalues():                    # XXX: very inefficient
            if all(row[i] == val for i, val in nonslice):
                yield tuple(row[i] for i in slices)

    def lookup(self, *args):
        "find index for these args"
        assert len(args) == self.ncols - 1                    # XXX: lookup doesn't want value?
        for idx, row in self.data.iteritems():                # XXX: very inefficient
            if row[:-1] == args:
                return idx             # stop on first

    def update(self, ix, args):
        "Update chart"
        self.data[ix] = args

    def insert(self, args):
        idx = self.next_id()
        self.update(idx, list(args))
        return idx

    def next_id(self):
        idx = self._id
        self._id += 1
        return idx


# TODO: (functor, idx) pairs aren't nice to look at.
def pretty(item):
    "Pretty print a term. Will retrieve the complete (ground) term for the chart."
    (fn, idx) = item
    row = chart[fn].data[idx]
    args = row[:-1]
    fn = ''.join(fn.split('/')[:-1])  # drop arity from name.
    return '%s%s' % (fn, tuple(args))    # TODO: need to recurse.

def prettify(x):

    if isinstance(x, tuple):
        return pretty(x)
    elif isinstance(x, list):
        return map(pretty, x)
    elif isinstance(x, Chart):
        return {pretty((x.name, k)): v for k,v in x.data.iteritems()}
    else:
        raise ValueError("Don't know what to do with %r" % x)


aggregator = defaultdict(Counter)
agenda = []


def run_agenda():
    "the main loop"
    while agenda:
        item = agenda.pop()
        new = aggregate(item)   # compute new value for item
        propagate(item, new)


def aggregate(item):
    print 'aggregate:', pretty(item), aggregator[item],

    val = 0.0
    for k,v in aggregator[item].iteritems():
        val += k*v                # value*multiplicity; TODO: use correct aggregator

    print 'result:', val

    return val


def delete(item, val):
    print 'TODO: implement deletion.'



def propagate(item, now):

    fn, idx = item

    was = chart[fn].data[idx][-1]  # last is value

    print 'propagate: %-30s   was %s  now %s' % (pretty(item), was, now)

    if was == now:
        return

    if was is not None:
        delete(item, was)

    chart[fn].data[idx][-1] = now
    aggregator[item][now] += 1

    # enqueue work to the agenda
    agenda.append(item)



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

    for i, word in enumerate('Papa ate the caviar with a spoon .'.split()):
        idx = chart['phrase/3'].insert((word, i, i + 1, None))
        propagate(('phrase/3', idx), 1)



execfile('examples/cky.dyna.plan')

papa_example()

run_agenda()

dump_charts()
