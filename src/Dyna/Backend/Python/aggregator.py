from __future__ import division

# TODO: codegen should produce specialized Term with inc/dec methods baked
# in. This seems nicer than having a separate aggregator object.

# TODO: aggregators might want a reference to the item they are associated with.

# TODO: we should never have a negative multiplicity

import operator
from collections import Counter
from utils import drepr, _repr, user_vars, isbool, true, false
from errors import AggregatorError
from stdlib import todyna


class NoAggregatorError(Exception):
    """
    raised when an item doesn't have an aggregator.
    """
    pass


class Aggregator(object):
    def fold(self):
        raise NotImplementedError('')
    def inc(self, _val, _ruleix, _variables):
        raise NotImplementedError('')
    def dec(self, _val, _ruleix, _variables):
        raise NotImplementedError('')
    def clear(self):
        raise NotImplementedError('')


NoAggregator = Aggregator()


class BAggregator(Counter, Aggregator):
#    def __init__(self):
#        super(BAggregator, self).__init__()
    def inc(self, val, _ruleix, _variables):
        self[val] += 1
    def dec(self, val, _ruleix, _variables):
        self[val] -= 1
    def fromkeys(self, *_):
        raise NotImplementedError("This method should never be called.")
    def empty(self):
        return not any(m > 0 for m in self.itervalues())


class ColonEquals(BAggregator):
    def inc(self, val, ruleix, _variables):
        self[ruleix, val] += 1
    def dec(self, val, ruleix, _variables):
        self[ruleix, val] -= 1
    def fold(self):
        vs = [v for v, m in self.iteritems() if m > 0]
        if vs:
            [i, v] = max(vs)
            vs = {v for (r, v) in vs if r == i}   # filter down to max rule index
            if len(vs) == 1:
                return v
            else:
                vs = list(vs)   # for stability
                vs.sort()
                raise AggregatorError('`:=` got conflicting values %s for rule index %s' % (vs, i))


class Equals(BAggregator):
    def inc(self, val, _ruleix, _variables):
        self[val] += 1
    def dec(self, val, _ruleix, _variables):
        self[val] -= 1
    def fold(self):
        vs = [v for v, cnt in self.iteritems() if cnt > 0]
        if len(vs) == 0:
            return
        if len(vs) == 1:
            return vs[0]
        else:
            vs.sort()   # for stability
            raise AggregatorError('`=` got conflicting values %s' % (vs,))


class DictEquals(BAggregator):

    def inc(self, val, _ruleix, variables):
        # I think we only want user variables
        vs = user_vars(variables)
        self[val, vs] += 1

    def dec(self, val, _ruleix, variables):
        vs = user_vars(variables)
        self[val, vs] -= 1

    def fold(self):
        if not self.empty():
            return todyna([dict(b + (('$val', val),)) for (val, b), cnt in self.iteritems() if cnt > 0])


class majority_equals(BAggregator):
    def fold(self):
        [(k,c)] = self.most_common(1)   # how are ties handled?
        if c > 0:
            return k


class mean_equals(BAggregator):
    def fold(self):
        # TODO: support negative multiplicity or throw an error
        s = [k*m for k, m in self.iteritems() if m > 0]
        if len(s):
            n = sum(m for _, m in self.iteritems() if m > 0)
            return reduce(operator.add, s) / n


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


class or_equals(BAggregator):
    def fold(self):
        s = [x for x, m in self.iteritems() if m > 0]
        if len(s):
            for val in s:
                if not isbool(val):
                    raise TypeError('%s is not Boolean.' % _repr(val))
            # TODO: can short circuit as soon as we get a true... but above we
            # check the types.. so we don't get the benefit.
            return todyna(any(s))


class colon_dash(BAggregator):
    def fold(self):
        s = [x for x, m in self.iteritems() if m > 0]
        for val in s:
            if val is not true:
                raise TypeError('%s is not true.' % _repr(val))
        # TODO: can short circuit as soon as we get a true... but above we check
        # the types.. so we don't get the benefit.
        if len(s):
            return true


class and_equals(BAggregator):
    def fold(self):
        s = [x for x, m in self.iteritems() if m > 0]
        if len(s):
            for val in s:
                if not isbool(val):
                    raise TypeError('%s is not Boolean.' % _repr(val))
            # TODO: can short circuit as soon as we get a false. but above we
            # check the types.. so we don't get the benfit
            for val in s:
                if val is false:
                    return false
            return true


class set_equals(BAggregator):
    def fold(self):
        s = {x for x, m in self.iteritems() if m > 0}
        if len(s):
            return todyna(s)


class bag_equals(BAggregator):
    def fold(self):
        if any(m > 0 for m in self.itervalues()):
            x = list(Counter(self).elements())
            x.sort()
            return todyna(x)


# map names to functions
defs = {
    'max=': max_equals,
    'min=': min_equals,
    '+=': plus_equals,
    '*=': times_equals,
    '&=': and_equals,
    ':-': colon_dash,
    '|=': or_equals,
    'majority=': majority_equals,
    'set=': set_equals,
    'bag=': bag_equals,
    'mean=': mean_equals,
}


def aggregator(name, term):
    "Create aggregator by ``name``."

    if name is None:
        return None

    elif name == ':=':
        return ColonEquals()

    elif name == '=':
        return Equals()

    elif name == 'dict=':
        return DictEquals()

    else:
        return defs[name]()
