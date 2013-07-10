from __future__ import division

# TODO: codegen should produce specialized Term with inc/dec methods baked
# in. This seems nicer than having a separate aggregator object.

# TODO: aggregators might want a reference to the item they are associated with.

import operator
from collections import Counter
from utils import drepr, _repr, user_vars
from errors import AggregatorError

"""
class Aggregator(object):
    def fold(self):
        raise NotImplementedError
    def inc(self, val, ruleix, variables):
        raise NotImplementedError
    def dec(self, val, ruleix, variables):
        raise NotImplementedError
    def clear(self):
        raise NotImplementedError
"""

class NoAggregatorError(Exception):
    """
    raised when an item doesn't have an aggregator.
    """
    pass


class Aggregator(object):
    def fold(self):
        raise AggregatorError("item doesn't have an aggregator.")
    def inc(self, _val, _ruleix, _variables):
        pass
    def dec(self, _val, _ruleix, _variables):
        pass
    def clear(self):
        pass

NoAggregator = Aggregator()

class BAggregator(Counter, Aggregator):
#    def __init__(self):
#        super(BAggregator, self).__init__()
    def inc(self, val, _ruleix, _variables):
        self[val] += 1
    def dec(self, val, _ruleix, _variables):
        self[val] -= 1
    def fromkeys(self, *_):
        assert False, "This method should never be called."


#class PlusEquals(object):
#    __slots__ = 'pos', 'neg'
#    def __init__(self):
#        self.pos = 0
#        self.neg = 0
#    def inc(self, val, ruleix, variables):
#        self.pos += val
#    def dec(self, val, ruleix, variables):
#        self.neg += val
#    def fold(self):
#        return self.pos - self.neg


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



#from collections import namedtuple
#class Result(namedtuple('Result', 'value variables')):
#    def __repr__(self):
#        return 'Result(value=%s, variables=%s)' % (_repr(self.value), drepr(dict(self.variables)))


class DictEquals(BAggregator):

    def inc(self, val, _ruleix, variables):
        # I think we only want user variables
        vs = user_vars(variables)
        self[val, vs] += 1

    def dec(self, val, _ruleix, variables):
        vs = user_vars(variables)
        self[val, vs] -= 1

    def fold(self):
        from stdlib import todyna
        return todyna([b + (('$val', v),) for (v, b), cnt in self.iteritems() if cnt > 0])


class majority_equals(BAggregator):
    def fold(self):
        [(k,c)] = self.most_common(1)
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

#class maxwithkey_equals(max_equals):
#    def fold(self):
#        m = max_equals.fold(self)
#        self.key = None
#        if m is not None:
#            if not hasattr(m, 'aslist') or len(m.aslist) != 2:
#                raise AggregatorError("argmax expects a pair of values")
#            self.key = m.aslist[1]
#            return m.aslist[0]

#class minwithkey_equals(min_equals):
#    def fold(self):
#        m = min_equals.fold(self)
#        self.key = None
#        if m is not None:
#            if not hasattr(m, 'aslist') or len(m.aslist) != 2:
#                raise AggregatorError("argmin expects a pair of values")
#            self.key = m.aslist[1]
#            return m.aslist[0]


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

#class and_equals(BAggregator):
#    def fold(self):
#        s = [k for k, m in self.iteritems() if m > 0]
#        if len(s):
#            return reduce(lambda x,y: x and y, s)

#class or_equals(BAggregator):
#    def fold(self):
#        s = [k for k, m in self.iteritems() if m > 0]
#        if len(s):
#            return reduce(lambda x,y: x or y, s)

class boolean_or_equals(BAggregator):
    def fold(self):
        s = [x for x, m in self.iteritems() if m > 0]
        if len(s):
            for val in s:
                if val is not True and val is not False:
                    raise TypeError('%s is not Boolean.' % _repr(val))

            return reduce(lambda x,y: x or y, s)

class boolean_and_equals(BAggregator):
    def fold(self):
        s = [x for x, m in self.iteritems() if m > 0]
        if len(s):
            for val in s:
                if val is not True and val is not False:
                    raise TypeError('%s is not Boolean.' % _repr(val))
            return reduce(lambda x,y: x and y, s)

class set_equals(BAggregator):
    def fold(self):
        from stdlib import todyna
        s = {x for x, m in self.iteritems() if m > 0}
        if len(s):
            return todyna(s)

class bag_equals(BAggregator):
    def fold(self):
        from stdlib import todyna
        return todyna(list(Counter(self).elements()))


# map names to functions
defs = {
    'max=': max_equals,
    'min=': min_equals,
    '+=': plus_equals,
    '*=': times_equals,
    '&=': boolean_and_equals,
    '|=': boolean_or_equals,
    ':-': boolean_or_equals,
    'majority=': majority_equals,
    'set=': set_equals,
    'bag=': bag_equals,
    'mean=': mean_equals,
#    'argmax=': maxwithkey_equals,
#    'argmin=': minwithkey_equals,
}

def aggregator(name, term):
    "Create aggregator by ``name``."

    if name is None:
        return None

    if name == ':=':
        return ColonEquals()

    elif name == '=':
        return Equals()

    elif name == 'dict=':
        return DictEquals()

    else:
        return defs[name]()
