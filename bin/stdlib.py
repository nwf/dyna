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

import os, sys, math, operator
from collections import defaultdict, Counter
from utils import red, green, blue, magenta


# Call indirection tables defines mathematical operators and the like.
call = {'*/2': operator.mul,
        '//2': operator.div,
        '-/2': operator.sub,
        '+/2': operator.add,

        '-/1': operator.neg,

#        '~/1': operator.inv,   # differs
#        '|/1': operator.or_,
#        '&/2': operator.and_,

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

        for row in self.data.values():                    # XXX: very inefficient
            if row[-1] is None:
                continue
            if all(row[i] == val for i, val in nonslice):
                yield tuple(row[i] for i in slices)

    def lookup(self, *args):
        "find index for these args"
        assert len(args) == self.ncols - 1                    # XXX: lookup doesn't want val?
        args = list(args)
        for idx, row in self.data.iteritems():                # XXX: very inefficient
            if row[:-1] == args:
                return idx             # stop on first

    def update(self, ix, args):
        "Update chart"
        assert len(args) == self.ncols and isinstance(args, list)
        self.data[ix] = args

    def insert(self, args):

        # debug: simple integrity check
        assert self.lookup(*args[:-1]) is None, '%s already in chart' % (args,)

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


# Update handler indirection -- a true hack. Allow us to have many handlers on
# the same functor/arity

# TODO: fuse update handlers instead of dispatching to each one independently.

def register(fn):
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
    >>> h("head", "val")
    ('OK', 'head', 'val')

    """

    def wrap(handler):
        register.handlers[fn].append(handler)
        # you can't call these guys directly. Must go thru handler
        # indirection table
        return None

    return wrap

# NOTE: global & mutable
register.handlers = defaultdict(list)



def initializer(_):

    def wrap(handler):
        initializer.handlers.append(handler)
        return None

    return wrap

initializer.handlers = []


def update_dispatcher((fn, idx), val):
    """
    Passes update to relevant handlers.
    """
    print 'dispatch', pretty((fn, idx)), '=', val
    for handler in register.handlers[fn]:
        print 'handler'
        handler((fn, idx), val)


def peel(fn, (fa, idx)):
    """
    Lookup `idx` in the intern table. Asserts that idx matches
    `functor_arity`. Returns arguments of term as a arity-tuple of intern idxs and
    constants.
    """
    assert fa == fn
    return chart[fn].data[idx][:-1]  # minus val


def build(fn, *args):
    idx = chart[fn].lookup(*args)   # lookup doesn't require val?
    if idx is None:
        idx = chart[fn].insert(args + (None,))   # don't know val yet.
    return (fn, idx)


_delete = False

def emit(item, val):
    (fn, idx) = item
    row = chart[fn].data[idx]
    was = row[-1]

    print (red if _delete else green) \
        % 'emit %s   (val %s; was: %s)' % (pretty(item), val, was)

    if _delete:
        aggregator[item][val] -= 1
    else:
        aggregator[item][val] += 1

    agenda.add(item)


aggregator = defaultdict(Counter)
agenda = set()


def run_agenda():
    "the main loop"
    while agenda:
        (fn, idx) = item = agenda.pop()

        print
        print 'pop', pretty(item)

        was = chart[fn].data[idx][-1]  # last cell is val

        if was is not None:
            delete(item, was)

        now = aggregate(item)   # compute new val for item

        print '    was %s  now %s' % (was, now)

        if was == now:
            print '    unchanged'
            return

        chart[fn].data[idx][-1] = now

        update_dispatcher(item, now)


def aggregate(item):
    print '    aggregate:', pretty(item), aggregator[item],
    val = 0.0
    for k,v in aggregator[item].iteritems():
        val += k*v                # val*multiplicity; TODO: use correct aggregator
    print 'result:', val
    return val


def delete(item, val):
    # XXX: very ugly handling of deletion
    global _delete
    _delete = True
    update_dispatcher(item, val)
    _delete = False


#def papa_example():
#
#    map(chart['rewrite/3'].insert,
#        [( "S",   "S",  ".", 1.),
#         ( "S",  "NP", "VP", 1.),
#         ("NP", "Det",  "N", 1.),
#         ("NP",  "NP", "PP", 1.),
#         ("VP",   "V", "NP", 1.),
#         ("VP",  "VP", "PP", 1.),
#         ("PP",   "P", "NP", 1.)])
#
#    map(chart['rewrite/2'].insert,
#        [( "NP",   "Papa", 1.),
#         (  "N", "caviar", 1.),
#         (  "N",  "spoon", 1.),
#         (  "V",    "ate", 1.),
#         (  "P",   "with", 1.),
#         ("Det",    "the", 1.),
#         ("Det",      "a", 1.)])
#
#    for i, word in enumerate('Papa ate the caviar with a spoon .'.split()):
#        idx = chart['phrase/3'].insert((word, i, i + 1, None))
#
#        emit(('phrase/3', idx), 1.0)
#
#execfile('examples/cky.dyna.plan')
#papa_example()


# 'examples/papa.dyna.plan'
[dyna] = sys.argv[1:]

cmd = """ghc -isrc Dyna.Backend.Python -e 'processFile "%s"' """ % dyna
assert 0 == os.system(cmd), 'command failed:\n\t' + cmd


execfile(dyna + '.plan')


for xxx in initializer.handlers:
    xxx()

run_agenda()

dump_charts()
