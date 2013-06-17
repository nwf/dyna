import sys
from IPython.core.ultratb import VerboseTB
from utils import parse_attrs
from config import dotdynadir


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

    print 'FATAL ERROR (%s): %s' % (etype.__name__, evalue)
    print 'Please report this error by emailing bugs@dyna.org. ' \
        'Please attach the following file %s' % crashreport.name


def enable_crash_handler():
    """
    Use our custom exception handler for handling uncaught exceptions.
    """
    sys.excepthook = exception_handler


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
