import re, os

from IPython.frontend.terminal.embed import InteractiveShellEmbed
ip = InteractiveShellEmbed(banner1 = 'Dropping into IPython\n')

black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


dynahome = os.getenv('DYNAHOME', '.')


def parse_sexpr(e):
    """
    Parse a string representing an s-expressions into lists-of-lists.

    based on implementation by George Sakkis
    http://mail.python.org/pipermail/python-list/2005-March/312004.html
    """
    e = re.compile('^\s*;.*?\n', re.M).sub('', e)  # remove comments

    es, stack = [], []
    for token in re.split(r'("[^"]*?"|[()])|\s+', e):
        if token == '(':
            new = []
            if stack:
                stack[-1].append(new)
            else:
                es.append(new)
            stack.append(new)
        elif token == ')':
            try:
                stack.pop()
            except IndexError:
                raise ValueError("Unbalanced right parenthesis: %s" % e)
        elif token:
            try:
                stack[-1].append(token)
            except IndexError:
                raise ValueError("Unenclosed subexpression (near %s)" % token)
    return es


def read_anf(e):
    x = parse_sexpr(e)

    def _g(x):
#        return [(var, val[0], val[1:]) for var, val in x]
        for var, val in x:
            if isinstance(val, list):
                yield (var, val[0], val[1:])
            else:
                yield (var, val, [])
    def g(x):
        return list(_g(x))

    for (agg, head, evals, unifs, [_,result]) in x:
        yield (agg,
               head,
               g(evals[1:]),
               g(unifs[1:]),
               result)


def notimplemented(*_,**__):
    raise NotImplementedError


from heapq import heapify, heappush, heappop

class prioritydict(dict):
    """Dictionary that can be used as a priority queue.

    Keys of the dictionary are items to be put into the queue, and values
    are their respective priorities. All dictionary methods work as expected.
    The advantage over a standard heapq-based priority queue is
    that priorities of items can be efficiently updated (amortized O(1))
    using code as 'thedict[item] = new_priority.'

    The 'smallest' method can be used to return the object with lowest
    priority, and 'pop_smallest' also removes it.

    The 'sorted_iter' method provides a destructive sorted iterator.

    This implemented is based on:

      Matteo Dell'Amico's implementation
      http://code.activestate.com/recipes/522995-priority-dict-a-priority-queue-with-updatable-prio/

         which is based on David Eppstein's implementation
         http://code.activestate.com/recipes/117228/

    """

    def __init__(self, *args, **kwargs):
        super(prioritydict, self).__init__(*args, **kwargs)
        self._rebuild_heap()

    def _rebuild_heap(self):
        self._heap = [(v, k) for k, v in self.iteritems()]
        heapify(self._heap)

    def smallest(self):
        """
        Return the item with the lowest priority.

        Raises IndexError if the object is empty.
        """

        heap = self._heap
        v, k = heap[0]
        while k not in self or self[k] != v:
            heappop(heap)
            v, k = heap[0]
        return k

    def pop_smallest(self):
        """
        Return the item with the lowest priority and remove it.

        Raises IndexError if the object is empty.
        """

        heap = self._heap
        v, k = heappop(heap)
        while k not in self or self[k] != v:
            v, k = heappop(heap)
        del self[k]
        return k

    def __setitem__(self, key, val):
        # We are not going to remove the previous value from the heap,
        # since this would have a cost O(n).

        super(prioritydict, self).__setitem__(key, val)

        if len(self._heap) < 2 * len(self):
            heappush(self._heap, (val, key))
        else:
            # When the heap grows larger than 2 * len(self), we rebuild it
            # from scratch to avoid wasting too much memory.
            self._rebuild_heap()

    setdefault = None
    update = None


def parse_attrs(fn):
    attrs = dict(re.findall('\s*(\S+):\s*(.*)\s*\n', fn.__doc__.strip()))
    if 'Span' in attrs:
        attrs['rule'] = rule_source(attrs['Span']).strip()
    return attrs


def rule_source(span):
    """
    Utility for retrieving source code for Parsec error message.
    """
    [(filename, bl, bc, el, ec)] = re.findall(r'(.*):(\d+):(\d+)-\1:(\d+):(\d+)', span)
    (bl, bc, el, ec) = map(int, [bl, bc, el, ec])

    with file(filename) as f:
        src = f.read()

    lines = [l + '\n' for l in src.split('\n')]

    rlines = lines[bl-1: el]

    if len(rlines) > 1:
        s = rlines[0][bc-1:]
        m = rlines[1:-1]
        e = rlines[-1][:ec]
        return s + ''.join(m) + e

    else:
        [line] = rlines
        return line[bc-1:ec]


if __name__ == '__main__':
    rule_source('examples/papa.dyna:4:1-examples/papa.dyna:4:47')
