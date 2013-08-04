import re
from external.path import path  # used by other modules
from IPython.frontend.terminal.embed import InteractiveShellEmbed
from config import dynahome, dotdynadir
from collections import namedtuple, defaultdict
from cStringIO import StringIO


def groupby(key, data):
    g = defaultdict(list)
    for x in data:
        g[key(x)].append(x)
    return dict(g)


def indent(x, indent=''):
    if isinstance(x, basestring):
        return re.compile('^(.*)$', flags=re.MULTILINE).sub(indent + r'\1', x)
    else:
        return [indent + y for y in x]


class _true(object):
    def __nonzero__(self):
        return True
    def __repr__(self):
        return 'true'
    def __eq__(self, other):
        return self is other
    def __cmp__(self, other):
        if other == 1:
            return -1
        return cmp(True, other)

class _false(object):
    def __nonzero__(self):
        return False
    def __repr__(self):
        return 'false'
    def __eq__(self, other):
        return self is other
    def __cmp__(self, other):
        if other == 0:
            return -1
        return cmp(False, other)

true = _true()
false = _false()
null = None

def isbool(x):
    return x is true or x is false


def _repr(x):
    #assert x is not True and x is not False, x
    if x is None:
        return 'null'
    elif isinstance(x, basestring):
        # dyna doesn't accept single-quoted strings
        return '"%s"' % repr(x)[1:-1].replace('"', r'\"')
    else:
        return repr(x)


def drepr(vs):
    return '{%s}' %  ', '.join('%s=%s' % (k, _repr(v)) for k,v in vs.iteritems())


def user_vars(variables):
    "Post process the variables past to emit (which passes them to aggregator)."
    # remove the 'u' prefix on user variables 'uX'
    # Note: We also ignore user variables with an underscore prefix
    return tuple((name[1:], val) for name, val in variables
                 if name.startswith('u') and not name.startswith('u_'))


# interactive IPython shell
ip = InteractiveShellEmbed(banner1 = 'Dropping into IPython\n')


def get_module(cmd, sub):
    try:
        exec 'from %s.%s import %s as m' % (cmd, sub, sub)
    except (ImportError, SyntaxError):
        return
    else:
        return m


red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(1,8))
bold = '\033[1m%s\033[0m'


# TODO: use fabulous colors
#from fabulous.color import red, green, yellow, blue, magenta, cyan, white, bold, underline


def hide_ugly_filename(x, replacement='<repl>'):
    p = dotdynadir + '[a-z0-9/.]+\.dyna\S*'
    return re.sub(p, replacement, x)


def lexer(term):
    return re.findall('"[^"]*"'               # string
                      "|'[^']+'"              # quoted functor
                      '|[a-z][a-zA-Z_0-9]*'   # functor
                      '|[A-Z_][a-zA-Z0-9_]*'  # variable
                      '|[(), \[\]|:]+'        # parens and comma
                      '|[^(), ]+', term)      # everything else


def subst(term, v):
    """
    >>> subst('f("asdf",*g(1,X, Y), X+1)', {'X': 1234})
    'f("asdf",*g(1,1234, Y), 1234+1)'

    >>> subst('f("asdf",*g(1,X, Y), XX+1)', {'X': 1234})
    'f("asdf",*g(1,1234, Y), XX+1)'

    >>> subst('f("asdf",*g(1,uX, Y), X_+1)', {'X': 1234})
    'f("asdf",*g(1,uX, Y), X_+1)'

    """
    assert isinstance(v, dict)
    return ''.join((_repr(v[x]) if x in v else x) for x in lexer(term))


#import signal
#from contextlib import contextmanager
#
#@contextmanager
#def interrupt_after():
#
#    def handler(signum, frame):
#        sys.stderr.write('^C')
#        handler.interrupted = True
#        return signal.SIG_IGN
#
#    handler.interrupted = False
#    signal.signal(signal.SIGINT, handler)
#
#    yield
#
#    signal.signal(signal.SIGINT, signal.default_int_handler)
#
#    if handler.interrupted:
#        raise KeyboardInterrupt


class ddict(dict):
    """
    Default Dict where the default function gets the key as an argument, unlike
    collections.defaultdict.
    """
    def __init__(self, f):
        self.f = f
        super(ddict, self).__init__()
    def __missing__(self, x):
        self[x] = y = self.f(x)
        return y


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


def pretty_print(t):
    print pretty(t)

def pretty(t, initialindent=0):
    "Pretty print tree as a tabbified s-expression."
    f = StringIO()
    out = f.write
    def pp(t, indent=initialindent, indentme=True):
        if indentme:
            out(' '*indent)
        if isinstance(t, basestring):                    # base case
            return out('%s' % t)
        if len(t) == 1:
            if t[0]:
                pp('%s' % t[0], indent, indentme)
            return
        label, children = t[0], t[1:]
        label = '%s' % label
        assert isinstance(label, basestring)
        out('(%s ' % label)
        n = len(children)
        for i, child in enumerate(children):
            pp(child, indent + len(label) + 2, i != 0)   # first child already indented
            if i != n-1:                                 # no newline after last child
                out('\n')
        out(')')
    pp(t)
    out('\n')
    return f.getvalue()


class ANF(namedtuple('ANF', 'span ruleix agg head evals unifs result')):

    @staticmethod
    def read(span, index, x):
        def _g(x):
            for var, val in x:
                if isinstance(val, list):
                    yield (var, val[0], val[1:])
                else:
                    yield (var, val, [])
        def g(x):
            return list(_g(x))

        [(agg, head, evals, unifs, [_, result])] = parse_sexpr(x)

        return ANF(span,
                   index,
                   agg,
                   head,
                   g(evals[1:]),
                   g(unifs[1:]),
                   result)


def read_anf(e):
    for span, ruleix, x in re.findall('^;; (.*)\n;; index (\d+)\n(\([\w\W]+?)\n(?:\n|$)', e, re.MULTILINE):
        yield ANF.read(span, int(ruleix), x)


def parse_attrs(fn):
    attrs = dict(re.findall('\s*(\S+):\s*(.*)\s*\n', fn.__doc__.strip()))
    if 'Span' in attrs:
        attrs['rule'] = span_to_src(attrs['Span']).strip()
    return attrs


def span_to_src(span, src=None):
    """
    Utility for retrieving source code for Parsec error message (there is
    nothing specific about rules)
    """
    try:
        [(filename, bl, bc, el, ec)] = re.findall(r'(.*):(\d+):(\d+)-\1:(\d+):(\d+)', span)
    except ValueError:
        return span

    (bl, bc, el, ec) = map(int, [bl, bc, el, ec])

    if not src:
        with file(filename) as f:
            src = f.read()

    lines = [l + '\n' for l in src.split('\n')]

    rlines = lines[bl-1: el]

    if len(rlines) > 1:
        s = rlines[0][bc-1:]
        m = rlines[1:-1]
        e = rlines[-1][:ec-1]
        return s + ''.join(m) + e

    else:
        [line] = rlines
        return line[bc-1:ec]
