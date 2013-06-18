from errors import notimplemented


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

#    def subst(self, v):
#        if self in v:
#            return v[self]
#        # TODO: this should go thru the Chart
#        return Term(self.fn, tuple(x if isconst(x) else x.subst(v) for x in self.args))


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


def isconst(x):
    return not isinstance(x, (Variable, Term))



class Variable(object):

    def __init__(self, name):
        self.fn = name
        self._val = None

    @property
    def value(self):
        return self.root._val

    @property
    def root(self):
        if isinstance(self._val, Variable):
            return self._val.root
        else:
            return self

    @value.setter
    def value(self, v):
        self._val = v

#    def __repr__(self, other):
#        if self.value is None:
#            return self.fn + '=' + self.root.fn
#        else:
#            return _repr(self.value)
#            return '%s=%s' % (self.fn, _repr(self.value))

    def __repr__(self):
        if self.value is None:
            return self.root.fn
        else:
            return _repr(self.value)

    def __eq__(self, other):
        if isinstance(other, Variable):

            if self.root is other.root:
                return True

            if self.value is not None:
                return other.value == self.value

            return False

        else:
            return other == self.value

#    def subst(self, v):
#        if self in v:
#            return v[self]
#        return self


def extend(v, x, t):
    """
    Extend valuation v with v[x] = t
    """
    v1 = v.copy()
    v1[x] = t
#    x.value = t
    return v1


def occurs_check(v, t):
    """
    Return true if variable ``v`` occurs anywhere in term ``t``."

    >>> [f,g,h,X,Y,Z] = map(symbol, ['f','g','h','X','Y','Z'])

    >>> occurs_check(X, f(g(h(X,Y),X)))
    True

    >>> occurs_check(Z, f(g(h(X,Y),X)))
    False

    """
    if v == t:
        return True
    elif isinstance(t, Term):
        return any(occurs_check(v, x) for x in t.args)
    return False


def unify_var(x, t, v):
    """
    Test if v can be extended with v[x] = t;
    In that case return the extention
    Else return None
    """
    if x in v:
        return _unify(v[x], t, v)
    elif occurs_check(x, t):
        return None
    else:
        return extend(v, x, t)


def unify(x,y):
    return _unify(x,y,{})


def _unify(x,y,v):
    """
    Find one valuation extending v and unifying x with y
    """
    if v is None:
        return None
    elif x == y:
        return v
    elif isinstance(x, Variable):
        return unify_var(x, y, v)
    elif isinstance(y, Variable):
        return unify_var(y, x, v)
    elif isinstance(x, Term) and isinstance(y, Term) and x.fn == y.fn:
        if len(x.args) == len(y.args):
            for a, b in zip(x.args, y.args):
                v = _unify(a, b, v)
            return v


def symbol(name):
    if name[0].isupper():
        return Variable(name)

    class Symbol(object):
        def __init__(self, name):
            self.name = name
        def __call__(self, *args):
            return Term('%s/%s' % (self.name, len(args)), args)
        def __str__(self):
            return self.name

    return Symbol(name)


#if __name__ == '__main__':
#
#    [f,g,h] = map(symbol, ['f','g','h'])
#    vs = [X,Y,Z] = map(symbol, ['X','Y','Z'])
#
#    def test(a, b):
#        for v in vs:
#            v.value = None
#
#        print
#        print 'unify %s and %s' % (a,b)
#        s = unify(a, b)
#        print '->', s
#        if s is None:
#            return
#
#        # apply new bindings
#        for v, now in s.iteritems():
#            print '%s [was: %r, now: %r]' % (v, v.value, now)
#            v.value = now
#
#        print a, b
#        assert a == b
#        assert repr(a) == repr(b), [a,b]
#
#        for v in vs:
#            v.value = None
#
#    test(f(X), f(g(h(X,Y),X)))
#    test(f(X), f(g(h(Z),"foo")))
#    test(f(X, Y), f("cat", 123))
#    test(f(X), f(X))
#    test(f(X), f(Y))
#    test("abc", "abc")
#
#    Z.value = 3
#    Y.value = Z
#    X.value = Y
#    print [X,Y,Z]
#    assert X.value == Y.value == Z.value == 3

