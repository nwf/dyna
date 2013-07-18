#!/usr/bin/env python
import re, sys, traceback
from interpreter import Interpreter
from repl import REPL
from cStringIO import StringIO
from utils import red, green, yellow, strip_comments


def extract(code):
    for block in re.compile('^> ', re.MULTILINE).split(code):
        cmd = []
        expect = []

        reading = True

        for i, line in enumerate(block.split('\n')):
            if (line.startswith('|') or i == 0) and reading:
                if line.startswith('|'):
                    line = ' ' + line[1:]
                cmd.append(line)
            else:
                reading = False
                expect.append(line)

        yield '\n'.join(cmd).strip(), '\n'.join(expect).strip()


def clean(x):
    # remove whitespace at end of line
    # remove ansi color codes
    return re.compile('(\s*)$', re.MULTILINE).sub('', re.sub('\033\[\d+m', '', strip_comments(x)).strip())


def run(code, out=None):

    if out is None:
        out = sys.stdout

    interp = Interpreter()
    repl = REPL(interp)
    errors = 0
    for cmd, expect in extract(code):

        if not clean(cmd):
            print >> out
            continue

        print >> out, yellow % '> %s' % cmd

        if clean(cmd) == '*resume*':
            repl.cmdloop()
            continue

        sys.stdout = x = StringIO()
        try:
            repl.onecmd(cmd)
        except:
            print >> out, red % traceback.format_exc()
        finally:
            sys.stdout = sys.__stdout__

        got = clean(x.getvalue())
        expect = clean(expect)

        if expect == '*ignore*':
            continue

        if expect != got:
            print >> out, green % expect
            print >> out, red % got
            errors += 1
        else:
            print >> out
            print >> out, got

        print >> out

    if not errors:
        print >> out, green % 'PASS!'
        return 0
    else:
        print >> out, yellow % '>>>', red % '%s errors' % errors
        return 1


if __name__ == '__main__':
    for filename in sys.argv[1:]:
        with file(filename) as f:
            run(f.read())
