import re, os

from IPython.frontend.terminal.embed import InteractiveShellEmbed
ip = InteractiveShellEmbed(banner1 = 'Dropping into IPython\n')

black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


def toANF(code, f='/tmp/tmp.dyna'):
    "Convert to ANF using Haskell implemention via system call."
    with file(f, 'wb') as tmp:
        tmp.write(code)
    os.system('rm -f %s.anf' % f)  # clean up any existing ANF output
    assert 0 == os.system("""dist/build/dyna/dyna --dump-anf="%s".anf --backend=none \"%s\" """ % (f,f)), \
        'failed to convert file.'
    with file('%s.anf' % f) as h:
        return h.read()


def parse_sexpr(e):
    """
    Parse a string representing an s-expressions into lists-of-lists.

    based on implementation by George Sakkis
    http://mail.python.org/pipermail/python-list/2005-March/312004.html
    """
    e = re.compile('^\s*;.*?\n', re.M).sub('', e)  # remove comments

    es, stack = [], []
    for token in re.split(r'([()])|\s+', e):
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
