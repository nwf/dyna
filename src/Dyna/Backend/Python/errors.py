import readline, sys
from IPython.core.ultratb import VerboseTB
from utils import parse_attrs
from config import dotdynadir


class DynaCompilerError(Exception):
    def __init__(self, msg, filename):
        self.filename = filename
        super(DynaCompilerError, self).__init__(msg)


class AggregatorError(Exception):
    pass



# TODO: we should package up all relevant state including compiler version,
# codegen output, interpreter state (possibly without the chart -- because it
# might be too big to email); input to repl.  This should all go into a tarball.
def crash_handler():
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

        show_traceback((etype, evalue, tb))

        for hook in crash_handler.hooks:
            hook()

        print 'Crash log available %s' % crashreport.name

    sys.excepthook = exception_handler

crash_handler.hooks = []



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


def rule_error_context():
    """
    Inspect the stack frame to extract local variable bindings of an update
    handler, rule initializer or backward chaining routine.

    >>> def f(x):
    ...     y = 3
    ...     return g(x)

    >>> def g(x):
    ...    return _(x)

    >>> def _(x):             # target frame
    ...    y = x + 1
    ...    z = 3 * 4
    ...    errorizer()

    >>> def errorizer():
    ...    a = 1
    ...    b = 0
    ...    c = a / b          # divide by zero error.
    ...    d = 3              # will be undefined, along with c
    ...    return 10

    >>> try:
    ...     print f(3)
    ... except ZeroDivisionError:
    ...     print rule_error_context()
    {'y': 4, 'x': 3, 'z': 12}

    """

    # Move to the frame where the exception occurred, which is often not the
    # same frame where the exception was caught.
    tb = sys.exc_info()[2]
    if tb is not None:
        while 1:
            if not tb.tb_next:
                break
            tb = tb.tb_next
        f = tb.tb_frame
    else:                             # no exception occurred
        f = sys._getframe()

    # get the stack frames
    stack = []
    while f:
        stack.append(f)
        f = f.f_back

    rule_frame = None
    for frame in stack:
        if frame.f_code.co_name == '_':   # find frame which looks like an update handler (it's name is at least '_')
            rule_frame = frame

    if rule_frame is not None:
        return dict(rule_frame.f_locals)
    return {}
