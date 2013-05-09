#!/usr/bin/env python

"""

 - "initializers" aren't just initializers, they are the fully-naive bottom-up
   inference rules.

 - TODO: routines from probing the chart.

 - XXX: maybe the chart should store pretty printed term and a reference to the
   aggregator (each item get's its own aggregator to avoid a hash lookup).

 - XXX: should we store value and aggregators separate from others columns? that
   is, separate the chart and intern table.

 - TODO: should probably make a Term object.

 - XXX: we should probably fuse update handlers instead of dispatching to each
   one independently.

 - TODO: deletion of a rule should be running the initializer for the rule in
   deletion mode.

 - TODO: hooks from introspection, eval, and prioritization.

"""

#from debug import ultraTB2; ultraTB2.enable()
#from debug import saverr; saverr.enable(editor=True)

import os, sys
from collections import defaultdict
from argparse import ArgumentParser
from utils import ip, red, green, blue, magenta
from defn import agg_bind


# TODO: as soon as we have safe names for these things we can get rid of this.
class chart_indirect(dict):
    def __missing__(self, key):
        print >> sys.stderr, 'creating chart indirect for:', key
        arity = int(key.split('/')[-1])
        c = self[key] = Chart(name = key, ncols = arity + 1)  # +1 for value
        return c


class aggregator_indirect(dict):
    def __missing__(self, item):
        a = agg_bind(item, agg_decl)
        self[item] = a
        return a


aggregator = aggregator_indirect()


chart = chart_indirect()

_delete = False
agenda = set()
#aggregator = defaultdict(Counter)
agg = {}
agg_decl = None # filled in after exec, this only here to satisfy lint checker.


def dump_charts(out=sys.stdout):
    print >> out
    print >> out, 'Charts'
    print >> out, '============'

    fns = chart.keys()
    fns.sort()

    for x in fns:
        print >> out, chart[x]
        print >> out


class Chart(object):

    def __init__(self, name, ncols):
        self.name = name
        self.ncols = ncols
        self.data = {}
        self._id = 0

    def __repr__(self):
        rows = [(r, pretty((self.name, i)), pretty(r[-1])) for i, r in self.data.items() if r[-1] is not None]
        x = '\n'.join('%-30s := %s' % (p, v) for _, p, v in sorted(rows))
        return '%s\n=================\n%s' % (self.name, x)

    def __getitem__(self, item):
        assert isinstance(item, tuple) and len(item) == self.ncols, \
            'item width mismatch: ncols %s, item %s' % (self.ncols, len(item))
        nonslice = [(i, val) for i, val in enumerate(item) if not isinstance(val, slice)]
        slices = [i for i, val in enumerate(item) if isinstance(val, slice)]

        # XXX: very inefficient
        for row in self.data.values():  # note: dict changes size during iteration
            if row[-1] is None:
                continue
            if all(row[i] == val for i, val in nonslice):
                yield tuple(row[i] for i in slices)

    def __setitem__(self, item, now):
        assert isinstance(item, tuple) and len(item) == self.ncols - 1
        nonslice = [(i, v) for i, v in enumerate(item) if not isinstance(v, slice)]
        for idx, row in self.data.iteritems():
            if all(row[i] == val for i, val in nonslice):
                item = (self.name, idx)
                was = self.data[idx][-1]
                delete(item, was)
                emit(item, now)
        go()

    def lookup(self, args):
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

    def insert(self, args, val):

        args = list(args)
        args.append(val)

        # debugging check: row is not already in chart.
        assert self.lookup(args[:-1]) is None, '%s already in chart' % (args,)

        idx = self.next_id()
        self.update(idx, args)
        return idx

    def next_id(self):
        idx = self._id
        self._id += 1
        return idx


def pretty(item):
    "Pretty print a term. Will retrieve the complete (ground) term from the chart."
    if not isinstance(item, tuple):
        return repr(item)
    (fn, idx) = item
    row = chart[fn].data[idx]
    args = row[:-1]
    fn = ''.join(fn.split('/')[:-1])  # drop arity from name.
    pretty_args = map(pretty, args)
    if not len(pretty_args):          # zero arity -> no parens.
        return fn
    return '%s(%s)' % (fn, ','.join(pretty_args))


def prettify(x):
    if isinstance(x, tuple):
        return pretty(x)
    elif hasattr(x, '__iter__'):
        return map(pretty, x)
    elif isinstance(x, Chart):
        return {pretty((x.name, k)): v for k,v in x.data.iteritems()}
    else:
        raise ValueError("Don't know what to do with %r" % x)


# Update handler indirection -- a temporary hack. Allow us to have many handlers
# on the same functor/arity. Eventually, we'll fuse handlers into one handler.

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

register.handlers = defaultdict(list)


def initializer(_):
    "Same idea as register"

    def wrap(handler):
        initializer.handlers.append(handler)
        return None

    return wrap

initializer.handlers = []


def update_dispatcher(item, val):
    """
    Passes update to relevant handlers.
    """
    if val is None:
        return
    (fn, _) = item
    print >> sys.stderr, 'dispatch', pretty(item), '=', val
    for handler in register.handlers[fn]:
        handler(item, val)


def peel(fn, item):
    """
    Find item's args in the appropriate chart. Assert that idx matches
    functor/arity, `fn`. Returns the arguments of term as a tuple of intern idxs
    and constants (possibly an empty tuple).
    """

    if fn == "true/0" :
      assert (item is True)
      return
    if fn == "false/0" :
      assert (item is False)
      return

    assert isinstance(item, tuple)
    (fa, idx) = item
    assert fa == fn
    return chart[fn].data[idx][:-1]  # minus val


def build(fn, *args):
    if fn == "true/0" : return True
    if fn == "false/0" : return False

    idx = chart[fn].lookup(args)
    if idx is None:
        idx = chart[fn].insert(args, None)   # don't know val yet.
    return (fn, idx)


def emit(item, val):

    print >> sys.stderr, (red if _delete else green) \
        % 'emit %s (val %s; curr: %s)' % (pretty(item), val, lookup(item))

    if _delete:
        aggregator[item].dec(val)
    else:
        aggregator[item].inc(val)

    agenda.add(item)


def delete(item, val):
    # XXX: very ugly handling of deletion by global variable; should probably
    # target only handler at a time, because this will get called more times
    # than it should.
    global _delete
    _delete = True
    update_dispatcher(item, val)
    _delete = False


def lookup(item):
    (fn, idx) = item
    return chart[fn].data[idx][-1]


def _go():
    "the main loop"

    while agenda:
        (fn, idx) = item = agenda.pop()

        print >> sys.stderr
        print >> sys.stderr, 'pop', pretty(item),

        was = lookup(item)
        now = aggregator[item].fold()

        print >> sys.stderr, 'was %s, now %s' % (was, now)

        if was == now:
            print >> sys.stderr, 'unchanged'
            continue

        if was is not None:
            delete(item, was)

        chart[fn].data[idx][-1] = now

        if now is not None:
            update_dispatcher(item, now)


def go():
    try:
        _go()
    except KeyboardInterrupt:
        pass
    finally:
        if argv.output is not None:
            if argv.output == "-": dump_charts(sys.stdout)
            else:
                with file(argv.output, 'wb') as f: dump_charts(f)
        else: dump_charts()


def dynac(f):
    cmd = """dist/build/dyna/dyna -B python -o "%s".plan "%s" """ % (f,f)
    assert 0 == os.system(cmd), 'command failed:\n\t' + cmd
    return f + '.plan'


def load(f, verbose=True):

    if verbose:
        with file(f) as h:
            print >> sys.stderr, h.read()

    # load generated code.
    execfile(f, globals())


def dump(code, filename='/tmp/tmp.dyna'):
    "Write code to file."
    with file(filename, 'wb') as f:
        f.write(code)
    return filename


def do(filename):
    "Compile, load, and execute dyna code."

    assert os.path.exists(filename)

    initializer.handlers = []    # XXX: do we really want to clear?

    load(dynac(filename))

    for init in initializer.handlers:   # assumes we have cleared
        init()

    go()


parser = ArgumentParser(description=__doc__)
parser.add_argument('source', help='Path to Dyna source file.')
parser.add_argument('-i', dest='interactive', action='store_true', help='Fire-up an IPython shell.')
parser.add_argument('-o', dest='output', help='Output chart.')

argv = parser.parse_args()

#if argv.output is None:
#    argv.output = argv.source + '.chart'

do(argv.source)

if argv.interactive:
    ip()
