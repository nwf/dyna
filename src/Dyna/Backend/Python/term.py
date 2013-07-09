from errors import notimplemented
from utils import _repr
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
        if other is None:
            return False
        if not isinstance(other, Term):
            return False
        return self.fn == other.fn and self.args == other.args

    def __cmp__(self, other):
        if other is None:
            return 1
        if not isinstance(other, Term):
            return 1
        return cmp((self.fn, self.args), (other.fn, other.args))

    def __repr__(self):
        "Pretty print a term. Will retrieve the complete (ground) term."

        if self.fn == '->/2':
            return '%s -> %s' % self.args

        fn = '/'.join(self.fn.split('/')[:-1])  # drop arity from name.
        if not self.args:
            return fn
        return '%s(%s)' % (fn, ','.join(map(_repr, self.args)))

#    def __getstate__(self):
#        return (self.fn, self.args, self.value, self.aggregator)

#    def __setstate__(self, state):
#        (self.fn, self.args, self.value, self.aggregator) = state

#    def __add__(self, _):
#        raise TypeError("Can't subtract terms.")

#    def __sub__(self, _):
#        raise TypeError("Can't add terms.")

#    def __mul__(self, _):
#        raise TypeError("Can't multiply terms.")

#    def __div__(self, _):
#        raise TypeError("Can't divide terms.")


class Cons(Term):

    def __init__(self, head, tail):
        if not (isinstance(tail, Cons) or tail is Nil):
            raise TypeError('Malformed list')
        self.head = head
        self.tail = tail
        Term.__init__(self, 'cons/2', (head, tail))
        self.aggregator = NoAggregator
        self.aslist = [self.head] + self.tail.aslist

    def __repr__(self):
        return '[%s]' % (', '.join(map(_repr, self.aslist)))

    def __contains__(self, x):
        return x in self.aslist

    def like_chart(self):
        for a in self.aslist:
            if not isinstance(a, Term):
                yield a, (None,), a
            else:
                yield a, (None,), a

    def __iter__(self):
        return iter(self.aslist)

    def __eq__(self, other):
        try:
            return self.aslist == other.aslist
        except AttributeError:
            return False

    def __hash__(self):
        return hash(tuple(self.aslist))

#    def __cmp__(self, other):
#        try:
#            return cmp(self.aslist, other.aslist)
#        except AttributeError:
#            return


class _Nil(Term):

    def __init__(self):
        Term.__init__(self, 'nil/0', ())
        self.aggregator = NoAggregator
        self.aslist = []

    def __repr__(self):
        return '[]'

    def __contains__(self, x):
        return False

    def like_chart(self):
        return iter([])

    def __iter__(self):
        return iter([])

    def __eq__(self, other):
        try:
            return self.aslist == other.aslist
        except AttributeError:
            return False

#    def __cmp__(self, other):
#        try:
#            return cmp(self.aslist, other.aslist)
#        except AttributeError:
#            return 1


Nil = _Nil()
