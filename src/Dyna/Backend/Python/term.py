from errors import notimplemented
from utils import _repr
from defn import Aggregator


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
#        if self is other:
#            return 0
        if other is None:
            return 1
        if not isinstance(other, Term):
            return 1
#        if self == other:
#            return 0
        return cmp((self.fn, self.args), (other.fn, other.args))

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

from term import Term

class Cons(Term):
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail
        assert isinstance(tail, (Cons, _Nil)), tail
        Term.__init__(self, 'cons/2', (head, tail))
        self.aggregator = Aggregator()
    def tolist(self):
        return [self.head] + self.tail.tolist()
    def __repr__(self):
        return repr(self.tolist())

    def __iter__(self):
#        return iter([(x,(x,),x) for x in self.tolist()])
        for a in self.tolist():

            if not isinstance(a, Term):
                yield a, (None,), a

            else:
                yield a, (None,), a

    def __eq__(self, other):
        try:
            return self.tolist() == other.tolist()
        except AttributeError:
            return False

class _Nil(Term):
    def __init__(self):
        Term.__init__(self, 'nil/0', ())
        self.aggregator = Aggregator()

    def tolist(self):
        return []
    def __repr__(self):
        return '[]'

Nil = _Nil()
