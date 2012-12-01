import re, os

black, red, green, yellow, blue, magenta, cyan, white = \
    map('\033[3%sm%%s\033[0m'.__mod__, range(8))


def convert(f):
    os.system('rm -f %s.anf' % f)  # clean up any existing ANF output

    assert 0 == os.system("""ghc -isrc Dyna.Analysis.NormalizeParseSelftest -e 'normalizeFile "%s"' """ % f), \
        'failed to convert file.'

    with file('%s.anf' % f) as h:
        return h.read()


def toANF(code, f='/tmp/tmp.dyna'):
    with file(f, 'wb') as tmp:
        tmp.write(code)
    return convert(tmp.name)

# by George Sakkis (gsakkis at rutgers.edu)
# http://mail.python.org/pipermail/python-list/2005-March/312004.html
def parse_sexpr(e):
    "If multiple s-expressions expected as output, set multiple to True."
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


def evalthings(t):
    if isinstance(t, str):
        return t
    else:
        return [evalthings(x) for x in t]


def read_anf(e):
    x = evalthings(parse_sexpr(e))

    def g(x):
        return [(var, val[0], val[1:]) for var, val in x]

    for (agg, head, side, evals, unifs, [_,result]) in x:
        yield (agg,
               head,
               side[1:],
               g(evals[1:]),
               g(unifs[1:]),
               result)
