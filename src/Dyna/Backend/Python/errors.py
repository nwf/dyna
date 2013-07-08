import readline, sys
from IPython.core.ultratb import VerboseTB
from utils import parse_attrs
from config import dotdynadir


class DynaCompilerError(Exception):
    pass


class AggregatorError(Exception):
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
                (exception, span, rule)
        super(DynaInitializerException, self).__init__(msg)



def crash_handler(interp):
    """
    Use our custom exception handler for handling uncaught exceptions.
    """

    def exception_handler(etype, evalue, tb):

        print 'FATAL ERROR (%s): %s' % (etype.__name__, evalue)

        # once for the log file.
        with file(dotdynadir / 'crash.log', 'wb') as crashreport:
            h = VerboseTB(color_scheme='Linux',
                          call_pdb=False,
                          ostream=crashreport,
                          long_header=True,
                          include_vars=True,
                          check_cache=None)
            h(etype, evalue, tb)


            # Dump the entirety of readline's history.  I do not think that
            # we can easily distinguish what is this session or a different
            # one, but this is more useful than nothing.
            #
            # XXX Well, that'd be great, except that it doesn't work.

            if interp is not None:
                if interp._repl is not None:
                    crashreport.write("REPL history:\n")
                    for ix in xrange(1,readline.get_current_history_length()):
                        crashreport.write("%d: %s\n" \
                            % (ix,readline.get_history_entry(ix)))

                # TODO: we should package up all relevant state including
                # compiler version, codegen output, interpreter state
                # (possibly without the chart -- because it might be too big
                # to email); input to repl.  This should all go into a
                # tarball.

        show_traceback((etype, evalue, tb))

        print 'Crash log available %s' % crashreport.name

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
