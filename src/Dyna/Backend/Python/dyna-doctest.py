#!/usr/bin/env python
import re, sys

sys.path.append('src/Dyna/Backend/Python')

from interpreter import Interpreter
from repl import REPL
from cStringIO import StringIO

from utils import red, green, yellow, strip_comments


def extract(code):
    for block in re.compile('^> ', re.MULTILINE).split(code):
        for cmd, expect in re.findall('(.*?)\n([\w\W]*)$', block):
            yield cmd, expect


def run(code):
    interp = Interpreter()
    repl = REPL(interp)
    errors = []
    for cmd, expect in extract(code):
        if not strip_comments(cmd).strip():
            print
            continue
        print yellow % '> %s' % cmd
        sys.stdout = x = StringIO()
        try:
            repl.onecmd(cmd)
        finally:
            sys.stdout = sys.__stdout__
        got = x.getvalue().strip()
        expect = expect.strip()
        if strip_comments(expect) != strip_comments(got):
            print green % expect
            print red % got
            errors.append(cmd, expect, got)
        else:
            print x.getvalue().rstrip()
        print

    if not errors:
        print green % 'PASS!'
    else:
        print red % '%s errors' % len(errors)
    print

if __name__ == '__main__':
    for filename in sys.argv[1:]:
        with file(filename) as f:
            run(f.read())
