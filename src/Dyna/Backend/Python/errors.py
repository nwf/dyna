import sys
from IPython.core.ultratb import VerboseTB
from utils import parse_attrs
from config import dotdynadir


class DynaCompilerError(Exception):
    pass


class DynaInitializerException(Exception):
    def __init__(self, exception, init):
        rule = parse_attrs(init)['rule']
        span = parse_attrs(init)['Span']

        if span.startswith(dotdynadir / 'tmp'):
            # don't show users tmp files create by the repl.
            msg = '%r in ininitializer for rule\n    %s' % \
                (exception, rule)

        else:
            msg = '%r in ininitializer for rule\n  %s\n        %s' % \
                (exception,
                 span,
                 rule)
        super(DynaInitializerException, self).__init__(msg)


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

    show_traceback((etype, evalue, tb))

    # TODO: we should package up all relevant state including compiler
    # version, codegen output, interpreter state (possibly without the
    # chart -- because it might be too big to email); input to repl.
    # This should all go into a tarball.

    if crash_handler.interp is not None:
        crash_handler.interp()

    print 'FATAL ERROR (%s): %s' % (etype.__name__, evalue)
    print 'Crash log available %s' % crashreport.name


def crash_handler():
    """
    Use our custom exception handler for handling uncaught exceptions.
    """
    sys.excepthook = exception_handler

# XXX: global state...
crash_handler.interp = None


def show_traceback(einfo=None):
    if not einfo:
        einfo = sys.exc_info()
    (etype, evalue, tb) = einfo
    # once for the user
    h = VerboseTB(color_scheme='Linux',
                  call_pdb=False,
                  ostream=None,
                  tb_offset=0,
                  long_header=False,
                  include_vars=False,
                  check_cache=None)
    h(etype, evalue, tb)


def notimplemented(*_,**__):
    raise NotImplementedError
