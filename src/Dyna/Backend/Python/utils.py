import re
from IPython.frontend.terminal.embed import InteractiveShellEmbed
from path import path
from subprocess import Popen, PIPE
from config import dynahome, dotdynadir
from collections import namedtuple
from cStringIO import StringIO



class _true(object):
    def __nonzero__(self):
        return True
    def __repr__(self):
        return 'true'

class _false(object):
    def __nonzero__(self):
        return False
    def __repr__(self):
        return 'false'

true = _true()
false = _false()
null = None

def isbool(x):
    return x is true or x is false




def _repr(x):

# TODO: this assertion should eventually hold.
#    assert x is not True and x is not False, x

    if x is True:
        return 'true'
    elif x is False:
        return 'false'

    elif x is None:
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


black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


_comments = re.compile('%.*$', re.MULTILINE)
def strip_comments(src):
    return _comments.sub('', src).strip()


def dynac(f, out, anf=None, compiler_args=()):
    """
    Run compiler on file, ``f``, write results to ``out``. Raises
    ``DynaCompilerError`` on failure.
    """
    from errors import DynaCompilerError

    f = path(f)
    if not f.exists():
        raise DynaCompilerError("File '%s' does not exist." % f)

    cmd = ['%s/dist/build/dyna/dyna' % dynahome,
           '-B', 'python', '-o', out, f]

    if anf is None:
        cmd += ['--dump-anf=' + out + '.anf']
    else:
        cmd += ['--dump-anf=' + anf]

    cmd += compiler_args

    p = Popen(cmd, stdout=PIPE, stderr=PIPE)

    stdout, stderr = p.communicate()
    if p.returncode:
        assert not stdout.strip(), [stdout, stderr]
        stderr = hide_ugly_filename(stderr)
        raise DynaCompilerError(stderr, f)


def hide_ugly_filename(x, replacement='<repl>'):
    p = dotdynadir + '[a-z0-9/.]+\.dyna\S*'
    return re.sub(p, replacement, x)


def lexer(term):
    return re.findall('"[^"]*"'               # string
                      '|[a-z][a-zA-Z_0-9]*'   # functor
                      '|[A-Z_][a-zA-Z0-9_]*'   # variable
                      '|[(), \[\]|]+'         # parens and comma
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


class ANF(namedtuple('ANF', 'lines ruleix agg head evals unifs result')):
    pass


def read_anf(e):
    def _g(x):
        for var, val in x:
            if isinstance(val, list):
                yield (var, val[0], val[1:])
            else:
                yield (var, val, [])
    def g(x):
        return list(_g(x))

    for lines, ruleix, anf in re.findall('^;; (.*)\n;; index (\d+)\n(\([\w\W]+?)\n(?:\n|$)', e, re.MULTILINE):
        for (agg, head, evals, unifs, [_,result]) in parse_sexpr(anf):
            yield ANF(lines,
                      int(ruleix),
                      agg,
                      head,
                      g(evals[1:]),
                      g(unifs[1:]),
                      result)


def parse_attrs(fn):
    attrs = dict(re.findall('\s*(\S+):\s*(.*)\s*\n', fn.__doc__.strip()))
    if 'Span' in attrs:
        attrs['rule'] = rule_source(attrs['Span']).strip()
    return attrs


def rule_source(span, src=None):
    """
    Utility for retrieving source code for Parsec error message.
    """
    [(filename, bl, bc, el, ec)] = re.findall(r'(.*):(\d+):(\d+)-\1:(\d+):(\d+)', span)
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
