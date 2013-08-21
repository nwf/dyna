"""
Interface to compiler.

TODO:
 - read parser state
 - manage temp dir/files
 - anf
 - recompile rule should give same index

"""

import re, os
from subprocess import Popen, PIPE

from utils import path
from config import dynahome, dotdynadir
from hashlib import sha1
from errors import DynaCompilerError
from utils import hide_ugly_filename, span_to_src


def dynac(f, out, anf=None, compiler_args=()):
    """
    Run compiler on file, ``f``, write results to ``out``. Raises
    ``DynaCompilerError`` on failure.
    """

    f = path(f)
    if not f.exists():
        raise DynaCompilerError("File '%s' does not exist." % f)

    cmd = ['%s/dist/build/dyna/dyna' % dynahome,
           '-B', 'python', '-o', out, f]

    if anf is not None:
        cmd += ['--dump-anf=' + anf]

    cmd += compiler_args

    p = Popen(cmd, stdout=PIPE, stderr=PIPE)

    stdout, stderr = p.communicate()
    if p.returncode:
        assert not stdout.strip(), [stdout, stderr]
        stderr = hide_ugly_filename(stderr, lambda m: '\n  %s\n' % span_to_src(m.group(0)))
        raise DynaCompilerError(stderr, f)


class Compiler(object):

    def __init__(self):
        self.files = []
        self.tmp = tmp = (dotdynadir / 'tmp' / str(os.getpid()))
        if tmp.exists():
            tmp.rmtree()
        tmp.makedirs_p()

    def dynac(self, filename):
        """
        Compile a file full of dyna code. Note: this routine does not pass along
        parser_state.
        """
        filename = path(filename)
        self.files.append(filename)
        out = self.tmp / filename.read_hexhash('sha1') + '.plan.py'
        #out = filename + '.plan.py'
        self.files.append(out)
        dynac(filename, out)
        return out

    def dynac_code(self, code, pstate):
        "Compile a string of dyna code."
        x = sha1()
        x.update(pstate)
        x.update(code)
        dyna = self.tmp / ('%s.dyna' % x.hexdigest())
        with file(dyna, 'wb') as f:
            f.write(pstate)  # include parser state if any.
            f.write(code)
        return self.dynac(dyna)

    @staticmethod
    def parser_state(bc, rix, agg, other):
        # TODO: this is pretty hacky. XREF:parser-state
        lines = [':-ruleix %d.' % rix]
        for fn in bc:
            [(fn, arity)] = re.findall('(.*)/(\d+)', fn)
            lines.append(":-backchain '%s'/%s." % (fn, arity))
        for fn, agg in agg.items():
            [(fn, arity)] = re.findall('(.*)/(\d+)', fn)
            if agg is not None:
                lines.append(":-iaggr '%s'/%s %s ." % (fn, arity, agg))
        lines.extend(':-%s %s.' % (k,v) for k,v in other)
        lines.append('\n')
        return '\n'.join(lines)

    @staticmethod
    def read_parser_state(parser_state):
        """
        TODO: This is pretty hacky we should have the codegen produce something
        easier to serialize/modify/unserialize. XREF:parser-state.
        """
        backchain = set()
        ruleix = None
        iaggr = {}
        other = []
        for k, v in re.findall('^:-\s*(\S+) (.*?)\s*\.$', parser_state, re.MULTILINE):
            if k == 'backchain':
                [(fn, arity)] = re.findall("'(.*?)'/(\d+)", v)
                backchain.add('%s/%s' % (fn, arity))
            elif k == 'iaggr':
                [(fn, arity, agg)] = re.findall("'(.*?)'/(\d+)\s*(.*)", v)
                iaggr['%s/%s' % (fn, arity)] = agg
            elif k == 'ruleix':
                ruleix = int(v)
            else:
                other.append((k,v))
        return backchain, ruleix, iaggr, other
