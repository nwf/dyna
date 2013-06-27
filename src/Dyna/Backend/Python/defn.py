from __future__ import division

# TODO: codegen should produce specialized Term with inc/dec methods baked
# in. This seems nicer than having a separate aggregator object.

import operator
from collections import Counter


class Aggregator(object):
    def fold(self):
        raise NotImplementedError
    def inc(self, val, ruleix, variables):
        raise NotImplementedError
    def dec(self, val, ruleix, variables):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError


class BAggregator(Counter, Aggregator):
#    def __init__(self):
#        super(BAggregator, self).__init__()
    def inc(self, val, ruleix, variables):
        self[val] += 1
    def dec(self, val, ruleix, variables):
        self[val] -= 1
    def fromkeys(self, *_):
        assert False, "This method should never be called."


class PlusEquals(object):
    __slots__ = 'pos', 'neg'
    def __init__(self):
        self.pos = 0
        self.neg = 0
    def inc(self, val, ruleix, variables):
        self.pos += val
    def dec(self, val, ruleix, variables):
        self.neg += val
    def fold(self):
        return self.pos - self.neg


class ColonEquals(BAggregator):
    def inc(self, val, ruleix, variables):
        self[ruleix, val] += 1
    def dec(self, val, ruleix, variables):
        self[ruleix, val] -= 1
    def fold(self):
        vs = [v for v, cnt in self.iteritems() if cnt > 0]
        if vs:
            return max(vs)[1]


def user_vars(variables):
    "Post process the variables past to emit (which passes them to aggregator)."
    # remove the 'u' prefix on user variables 'uX'
    # Note: We also ignore user variables with an underscore prefix
    return tuple((name[1:], val) for name, val in variables
                 if name.startswith('u') and not name.startswith('u_'))


from term import _repr
def drepr(vs):
    return '{%s}' %  ', '.join('%s=%s' % (k, _repr(v)) for k,v in vs.iteritems())

from collections import namedtuple
class Result(namedtuple('Result', 'value variables')):
    def __repr__(self):
        return 'Result(value=%s, variables=%s)' % (_repr(self.value), drepr(dict(self.variables)))


class DictEquals(BAggregator):

    def inc(self, val, ruleix, variables):
        # I think we only want user variables -- XXX: are we guaranteed to have
        # all of the user variables?
        vs = user_vars(variables)
        self[val, vs] += 1

    def dec(self, val, ruleix, variables):
        vs = user_vars(variables)
        self[val, vs] -= 1

    def fold(self):
        return tuple(Result(v, b) for (v, b), cnt in self.iteritems() if cnt > 0)


class majority_equals(BAggregator):
    def fold(self):
        [(k,_)] = self.most_common(1)
        return k

class max_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return max(s)

class min_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return min(s)

class plus_equals(BAggregator):
    def fold(self):
        s = [k*m for k, m in self.iteritems() if m != 0]
        if len(s):
            return reduce(operator.add, s)

class times_equals(BAggregator):
    def fold(self):
        s = [k**m for k, m in self.iteritems() if m != 0]
        if len(s):
            return reduce(operator.mul, s)

class and_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return reduce(lambda x,y: x and y, s)

class or_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return reduce(lambda x,y: x or y, s)

class b_and_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return reduce(operator.and_, s)

class b_or_equals(BAggregator):
    def fold(self):
        s = [k for k, m in self.iteritems() if m > 0]
        if len(s):
            return reduce(operator.or_, s)

class set_equals(BAggregator):
    def fold(self):
        s = {x for x, m in self.iteritems() if m > 0}
        if len(s):
            return s

class bag_equals(BAggregator):
    def fold(self):
        return Counter(self)


# map names to functions
defs = {
    'max=': max_equals,
    'min=': min_equals,
    '+=': plus_equals,
    '*=': times_equals,
    'and=': and_equals,
    'or=': or_equals,
    '&=': b_and_equals,
    '|=': b_or_equals,
    ':-': or_equals,
    'majority=': majority_equals,
    'set=': set_equals,
    'bag=': bag_equals,
}

def aggregator(name):
    "Create aggregator by ``name``."

    if name is None:
        return None

    if name == ':=':
        return ColonEquals()

    elif name == 'dict=':
        return DictEquals()

    else:
        return defs[name]()
