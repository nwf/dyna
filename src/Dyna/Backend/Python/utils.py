import re, sys
from subprocess import Popen, PIPE
from IPython.frontend.terminal.embed import InteractiveShellEmbed
from IPython.core.ultratb import VerboseTB
from config import dynahome, dotdynadir
import signal
from contextlib import contextmanager


# interactive IPython shell
ip = InteractiveShellEmbed(banner1 = 'Dropping into IPython\n')


black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


class DynaCompilerError(Exception):
    pass


#class AggregatorConflict(Exception):
#    def __init__(self, key, expected, got):
#        msg = "Aggregator conflict %r was %r trying to set to %r." \
#            % (key, expected, got)
#        super(AggregatorConflict, self).__init__(msg)


class DynaInitializerException(Exception):
    def __init__(self, exception, init):
        msg = '%r in ininitializer for rule\n  %s\n        %s' % \
            (exception,
             parse_attrs(init)['Span'],
             parse_attrs(init)['rule'])
        super(DynaInitializerException, self).__init__(msg)


def dynac(f, out):
    """
    Run compiler on file, ``f``, write results to ``out``. Raises
    ``DynaCompilerError`` on failure.
    """
    p = Popen(['%s/dist/build/dyna/dyna' % dynahome,
               '-B', 'python', '-o', out, f], stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    if p.returncode:
        assert not stdout.strip(), [stdout, stderr]
        raise DynaCompilerError(stderr)


def exception_handler(etype, evalue, tb):

    # once for the log file.
    with file(dotdynadir / 'crash.log', 'wb') as crashreport:
        h = VerboseTB(color_scheme='Linux',
                      call_pdb=False,
                      ostream=crashreport,
                      long_header=True,
                      include_vars=True,
                      check_cache=None)
        h(etype, evalue, tb)

    # once for the user
    h = VerboseTB(color_scheme='Linux',
                  call_pdb=False,
                  ostream=None,
                  tb_offset=0,
                  long_header=False,
                  include_vars=False,
                  check_cache=None)
    h(etype, evalue, tb)

    # TODO: we should package up all relevant state including compiler
    # version, codegen output, interpreter state (possibly without the
    # chart -- because it might be too big to email); input to repl.
    # This should all go into a tarball.

    print 'FATAL ERROR (%s): %s' % (etype.__name__, evalue)
    print 'Please report this error by emailing bugs@dyna.org. ' \
        'Please attach the following file %s' % crashreport.name


def enable_crash_handler():
    """
    Use our custom exception handler for handling uncaught exceptions.
    """
    sys.excepthook = exception_handler


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



def notimplemented(*_,**__):
    raise NotImplementedError


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


if __name__ == '__main__':
    #rule_source('examples/papa.dyna:4:1-examples/papa.dyna:4:47')
    import sys

    def t(xs):
        if isinstance(xs, basestring):
#            return '"%s"' % xs
            return xs
        else:
            assert len(xs) > 1
            if len(xs) == 2:
                [sym, a] = map(t, xs)
#                return '&t(%s)' % ', '.join(t(x) for x in xs)
                return [sym, a]
            elif len(xs) == 3:
                [sym, a, b] = map(t, xs)
#                return '&t(%s, %s, %s)' % (sym, a, b)
                return [sym, a, b]
            else:
                [sym, a] = t(xs[0]), t(xs[1])
                rest = t(['@' + xs[0]] + xs[2:])
#                return '&t(%s, %s, %s)' % (sym, a, rest)
                return [sym, a, rest]


    def check_binary(x):
        if isinstance(x, basestring):
            return True
        elif len(x) in (2, 3):
            return all(map(check_binary, x))
        else:
            return False

    from cStringIO import StringIO

    def pretty(t, initialindent=0):
        "Pretty print tree as a tabbified s-expression."
        f = StringIO()
        out = f.write
        def pp(t, indent=initialindent, indentme=True):
            if indentme:
                out(' '*indent)
            if isinstance(t, basestring):                    # base case
                return out('"%s"' % t)
            if len(t) == 1:
                if t[0]:
                    pp('"%s"' % t[0], indent, indentme)
                return
            label, children = t[0], t[1:]

            label = '"%s"' % label

            assert isinstance(label, basestring)
            out('&t(%s, ' % label)
            n = len(children)
            for i, child in enumerate(children):
                pp(child, indent + len(label) + 5, i != 0)   # first child already indented
                if i != n-1:                                 # no newline after last child
                    out(',\n')
            out(')')
        pp(t)
        out('\n')
        return f.getvalue()

    for i, [x] in enumerate(parse_sexpr(sys.stdin.read())):
        btree = t(x)

        assert check_binary(btree)
        print
        print 'sentence(%s) :=\n%s.' % (i, pretty(btree, 4).rstrip())
        print
