from collections import defaultdict
from aggregator import aggregator
from term import Term
from utils import _repr, true


class Chart(object):

    def __init__(self, name, arity, agg_name):
        self.name = name
        self.arity = arity
        self.intern = {}   # args -> term
        self.ix = [defaultdict(set) for _ in xrange(arity)]
        self.agg_name = agg_name

    def new_aggregator(self, term):
        return aggregator(self.agg_name, term)

    def set_aggregator(self, agg):
        self.agg_name = agg
        for item in self.intern.values():
            assert item.value is None, [item, item.value, item.aggregator]   # shouldn't change aggregator when non-null.
            item.aggregator = self.new_aggregator(item)

    def __repr__(self):
        rows = [term for term in self.intern.values() if term.value is not None]
        if not rows:
            return ''

        heading = [self.name, '='*len(self.name)]

        # special handing or-equals aggregators -- only list true facts (and errors)
        if self.agg_name == ':-':
            lines = []
            for term in sorted(rows):
                if term.value is true:
                    lines.append('%s.' % _repr(term))
                elif term.value:  # e.g. $error
                    lines.append('%s = %s.' % (_repr(term), _repr(term.value)))
            if self.arity != 0:
                lines = heading + lines # heading
            return '\n'.join(lines)

        if self.arity == 0:
            [term] = rows
            return '%s = %s.' % (term, _repr(term.value))

        p = [(_repr(term), _repr(term.value)) for term in sorted(rows)]
        lines = []
        terms, values = zip(*p)
        widths = map(len, terms)
        fmt = '%%-%ds = %%s.' % min(max(widths), 40)
        for term, value in zip(terms, values):
            lines.append(fmt % (term, value))
        return '\n'.join(heading + lines)

    def __getitem__(self, s):
        assert len(s) == self.arity + 1, \
            'Chart %r: item width mismatch: arity %s, item %s' % (self.name, self.arity, len(s))

        args, val = s[:-1], s[-1]

        assert val is not None

        # filter set of candidates by each bound argument
        b = [(ix[x]) for (ix, x) in zip(self.ix, args) if not isinstance(x, slice)]

        if len(b) == 0:
            # all arguments are free.
            candidates = self.intern.values()

        elif len(b) == 1:
            candidates = list(b[0])

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
            term.aggregator = self.new_aggregator(term)
            # index new term
            for i, x in enumerate(args):
                self.ix[i][x].add(term)
            return term
