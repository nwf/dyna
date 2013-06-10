from collections import defaultdict
from utils import notimplemented


# TODO: codegen should output a derived Term instance for each functor
class Term(object):

    __slots__ = 'fn args value aggregator'.split()

    def __init__(self, fn, args):
        self.fn = fn
        self.args = args
        self.value = None
        self.aggregator = None

    def __cmp__(self, other):
        if other is None:
            return 1
        if not isinstance(other, Term):
            return 1
        return cmp((self.fn, self.args), (other.fn, other.args))

    # default hash and eq suffice because we intern
    #def __hash__(self):
    #def __eq__(self):

    def __repr__(self):
        "Pretty print a term. Will retrieve the complete (ground) term."
        fn = '/'.join(self.fn.split('/')[:-1])  # drop arity from name.
        if not self.args:
            return fn
        return '%s(%s)' % (fn, ','.join(map(_repr, self.args)))

    def __getstate__(self):
        return (self.fn, self.args, self.value, self.aggregator)

    def __setstate__(self, state):
        (self.fn, self.args, self.value, self.aggregator) = state

    __add__ = __sub__ = __mul__ = notimplemented


def _repr(x):
    if x is True:
        return 'true'
    elif x is False:
        return 'false'
    elif x is None:
        return 'null'
    elif isinstance(x, basestring):
        # dyna doesn't accept single-quoted strings
        return '"%s"' % x.replace('"', r'\"')
    else:
        return repr(x)


class Chart(object):

    def __init__(self, name, arity, new_aggregator):
        self.name = name
        self.arity = arity
        self.intern = {}   # args -> term
        self.ix = [defaultdict(set) for _ in xrange(arity)]
        self.new_aggregator = new_aggregator

    def __repr__(self):
        rows = [term for term in self.intern.values() if term.value is not None]
        x = '\n'.join('%-30s := %s' % (term, _repr(term.value)) for term in sorted(rows))
        return '%s\n=================\n%s' % (self.name, x)

    def __getitem__(self, s):
        assert len(s) == self.arity + 1, \
            'Chart %r: item width mismatch: arity %s, item %s' % (self.name, self.arity, len(s))

        args, val = s[:-1], s[-1]

        assert val is not None

        # filter set of candidates by each bound argument
        b = [(ix[x]) for (ix, x) in zip(self.ix, args) if not isinstance(x, slice)]

        if len(b) == 0:
            # all arguments are free.
            candidates = self.intern.itervalues()

        elif len(b) == 1:
            candidates = iter(b[0])

        else:
            b.sort(key=len)           # start with smaller ones
            candidates = b[0] & b[1]
            for ix in b[2:]:
                candidates &= ix
            candidates = iter(candidates)

        # handle the value column separately because we don't index it yet.
        if isinstance(val, slice):
            for term in candidates:
                if term.value is not None:
                    yield term, term.args, term.value
        else:
            for term in candidates:
                if term.value == val:
                    yield term, term.args, term.value

    def insert(self, args):        # TODO: rename
        try:
            return self.intern[args]
        except KeyError:
            self.intern[args] = term = Term(self.name, args)
            term.aggregator = self.new_aggregator()
            # index new term
            for i, x in enumerate(args):
                self.ix[i][x].add(term)
            return term
