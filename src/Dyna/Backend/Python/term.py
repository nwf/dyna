from utils import _repr, true, false
from aggregator import NoAggregator


# TODO: codegen should output a derived Term instance for each functor
class Term(object):

    __slots__ = 'fn args value aggregator'.split()

    def __init__(self, fn, args):
        self.fn = fn
        self.args = args
        self.value = None
        self.aggregator = None

    def __eq__(self, other):
        return self is other

    def __cmp__(self, other):
        if self is other:
            return 0
        try:
            return cmp((self.fn, self.args), (other.fn, other.args))
        except AttributeError:
            return 1

    def __repr__(self):
        "Pretty print a term. Will retrieve the complete (ground) term."
        fn = '/'.join(self.fn.split('/')[:-1])  # drop arity from name.
        if not self.args:
            return fn
        return '%s(%s)' % (fn, ','.join(map(_repr, self.args)))


class NoIntern(Term):
    "Mix-in which adds hash and equality method for terms which aren't interned."
    def __eq__(self, other):
        try:
            return (self.fn, self.args) == (other.fn, other.args)
        except AttributeError:
            return False
    def __hash__(self):
        return hash((self.fn, self.args))


class Cons(NoIntern, Term):

    def __init__(self, head, tail):
        if not (isinstance(tail, Cons) or tail is Nil):
            raise TypeError('Malformed list')
        self.head = head
        self.tail = tail
        Term.__init__(self, 'cons/2', (head, tail))
        self.aggregator = NoAggregator
        self.aslist = [self.head] + self.tail.aslist

    def __cmp__(self, other):
        try:
            if other.fn == 'cons/2':
                return cmp(self.aslist, other.aslist)   # faster
            else:
                return cmp(self.fn, other.fn)
        except AttributeError:
            return 1

    def __repr__(self):
        return '[%s]' % (', '.join(map(_repr, self.aslist)))

    def like_chart(self):
        for a in self.aslist:
            if not isinstance(a, Term):
                yield a, (None,), a
            else:
                yield a, (None,), a

    def __iter__(self):
        return iter(self.aslist)


class Error(NoIntern, Term):
    def __init__(self):
        Term.__init__(self, '$error/0', ())


class _Nil(Term):

    def __init__(self):
        Term.__init__(self, 'nil/0', ())
        self.aggregator = NoAggregator
        self.aslist = []

    def __repr__(self):
        return '[]'

    def like_chart(self):
        return iter([])

    def __iter__(self):
        return iter([])


Nil = _Nil()


class MapsTo(NoIntern, Term):
    def __init__(self, k, v):
        super(MapsTo, self).__init__('->/2', (k, v))
    def __repr__(self):
        return '%s -> %s' % tuple(map(_repr, self.args))
