# TODO: codegen should produce specialized Term with inc/dec methods baked
# in. This seems nicer than having a separate aggregator object.

import operator
from collections import Counter


class Aggregator(object):
    def __init__(self, name):
        self.name = name
    def fold(self):
        raise NotImplementedError
    def inc(self, val, ruleix, variables):
        raise NotImplementedError
    def dec(self, val, ruleix, variables):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError
    def __repr__(self):
        return 'Aggregator(%r)' % (self.name)


class BAggregator(Counter, Aggregator):
    def __init__(self, name, folder):
        self.folder = folder
        Aggregator.__init__(self, name)
        Counter.__init__(self)
    def fold(self):
        return self.folder(self)
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
    return tuple((name[1:], val) for name, val in variables.items()
                 if name.startswith('u') and not name.startswith('u_'))


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
        return list((x[0], dict(x[1])) for x, cnt in self.iteritems())


def majority_equals(a):
    [(k,_)] = a.most_common(1)
    return k

def max_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return max(s)

def min_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return min(s)

def plus_equals(a):
    s = [k*m for k, m in a.iteritems() if m != 0]
    if len(s):
        return reduce(operator.add, s)

def times_equals(a):
    s = [k**m for k, m in a.iteritems() if m != 0]
    if len(s):
        return reduce(operator.mul, s)

def and_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(lambda x,y: x and y, s)

def or_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(lambda x,y: x or y, s)

def b_and_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(operator.and_, s)

def b_or_equals(a):
    s = [k for k, m in a.iteritems() if m > 0]
    if len(s):
        return reduce(operator.or_, s)

def set_equals(a):
    s = {x for x, m in a.iteritems() if m > 0}
    if len(s):
        return s

def bag_equals(a):
    return Counter(a)


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
        return ColonEquals(name, folder=None)

#    elif name == '+=':
#        return PlusEquals()

    elif name == 'dict=':
        return DictEquals(name, folder=None)

    else:
        return BAggregator(name, defs[name])
