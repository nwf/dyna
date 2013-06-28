#!/usr/bin/env python
import re, sys

sys.path.append('src/Dyna/Backend/Python')

from interpreter import Interpreter
from repl import REPL
from cStringIO import StringIO

from utils import red, green


def extract(code):
    for block in re.compile('^:- ', re.MULTILINE).split(code):
        for cmd, expect in re.findall('(.*?)\n([\w\W]*)$', block):
            yield cmd, expect


def run(code):
    interp = Interpreter()
    repl = REPL(interp)

    for cmd, expect in extract(code):
        print ':-', cmd
        sys.stdout = x = StringIO()
        try:
            repl.onecmd(cmd)
        finally:
            sys.stdout = sys.__stdout__
        got = x.getvalue().strip()
        expect = expect.strip()
        if expect != got:
            print green % expect
            print red % got
        else:
            print x.getvalue().rstrip()
        print


if __name__ == '__main__':
    for filename in sys.argv[1:]:
        with file(filename) as f:
            run(f.read())
