import os, sys
import cmd, readline
import interpreter
from utils import blue, yellow, green, magenta, ip, DynaCompilerError, AggregatorConflict
from chart import _repr
from config import dotdynadir
import debug


class REPL(cmd.Cmd, object):

    def __init__(self, interp, hist):
        self.interp = interp
        cmd.Cmd.__init__(self)
        self.hist = hist
        if not os.path.exists(hist):
            readline.clear_history()
            with file(hist, 'wb') as f:
                f.write('')
        readline.read_history_file(hist)
        self.do_trace('off')
        self.lineno = 0

    @property
    def prompt(self):
        return ':- ' #% self.lineno

    def do_rules(self, _):
        self.interp.dump_rules()

    def do_retract_rule(self, idx):
        self.interp.retract_rule(idx)

    def do_retract_item(self, item):
        self.interp.retract_item(item)

    def do_exit(self, _):
        readline.write_history_file(self.hist)
        return -1

    def do_EOF(self, args):
        "Exit on end of file character ^D."
        print 'exit'
        return self.do_exit(args)

    def precmd(self, line):
        """
        This method is called after the line has been input but before it has
        been interpreted. If you want to modify the input line before execution
        (for example, variable substitution) do it here.
        """
        return line

    def postcmd(self, stop, line):
        self.lineno += 1
        return stop

    def do_chart(self, _):
        self.interp.dump_charts()

    def emptyline(self):
        """Do nothing on empty input line"""
        pass

    def do_ip(self, _):
        ip()

    def do_trace(self, args):
        if args == 'on':
            self.interp.trace = sys.stdout
        elif args == 'off':
            self.interp.trace = file(os.devnull, 'w')
        else:
            print 'Did not understand argument %r please use (on or off).' % args

    def do_debug(self, line):
        with file(dotdynadir / 'tmp.dyna', 'wb') as f:
            f.write(line)
        debug.main(f.name)

    def do_query(self, line):

        if line.endswith('.'):
            print "Queries don't end with a dot."
            return

        query = 'out(%s) dict= %s.' % (self.lineno, line)

        self.default(query)

        try:
            [(_, (_, results))] = self.interp.chart['out/1'][self.lineno,:]
        except ValueError:
            print 'No results.'
            return

        for val, bindings in results:
            print '   ', val, 'when', bindings
        print

    def default(self, line):
        """
        Called on an input line when the command prefix is not recognized.  In
        that case we execute the line as Python code.
        """
        line = line.strip()
        if not line.endswith('.'):
            print "ERROR: Line doesn't end with period."
            return
        try:
            src = self.interp.dynac_code(line)   # might raise DynaCompilerError
            changed = self.interp.do(src)        # throws AggregatorConflict

        except AggregatorConflict as e:
            print 'AggregatorConflict:', e
            print '> new rule(s) were not added to program.'

        except DynaCompilerError as e:
            print 'DynaCompilerError:'
            print e
            print '> new rule(s) were not added to program.'

        else:
            self._changed(changed)

    def _changed(self, changed):
        if not changed:
            return
        print '============='
        for x, v in sorted(changed.items()):
            print '%s := %s' % (x, _repr(v))
        print
        self.interp.dump_errors()

    def do_draw(self, _):
        self.interp.draw()

    def cmdloop(self, _=None):
        try:
            super(REPL, self).cmdloop()
        except KeyboardInterrupt:
            print '^C'
            self.cmdloop()
        except Exception as e:
            readline.write_history_file(self.hist)
            raise e
