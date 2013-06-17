import re, sys
from subprocess import Popen, PIPE
from IPython.frontend.terminal.embed import InteractiveShellEmbed
from config import dynahome, dotdynadir
import signal
from contextlib import contextmanager


# interactive IPython shell
ip = InteractiveShellEmbed(banner1 = 'Dropping into IPython\n')


black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))



def dynac(f, out):
    """
    Run compiler on file, ``f``, write results to ``out``. Raises
    ``DynaCompilerError`` on failure.
    """
    from errors import DynaCompilerError
    p = Popen(['%s/dist/build/dyna/dyna' % dynahome,
               '-B', 'python', '-o', out, f], stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    if p.returncode:
        assert not stdout.strip(), [stdout, stderr]
        raise DynaCompilerError(stderr)


@contextmanager
def interrupt_after():

    def handler(signum, frame):
        sys.stderr.write('^C')
        handler.interrupted = True
        return signal.SIG_IGN

    handler.interrupted = False
    signal.signal(signal.SIGINT, handler)

    yield

    signal.signal(signal.SIGINT, signal.default_int_handler)

    if handler.interrupted:
        raise KeyboardInterrupt


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
        e = rlines[-1][:ec]
        return s + ''.join(m) + e

    else:
        [line] = rlines
        return line[bc-1:ec]
