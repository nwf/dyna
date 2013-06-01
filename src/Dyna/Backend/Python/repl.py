import os, sys
import cmd
import readline

import interpreter


class REPL(cmd.Cmd, object):

    def __init__(self, hist):
        cmd.Cmd.__init__(self)
        self.prompt = ":- "
        self.hist = hist
        if not os.path.exists(hist):
            with file(hist, 'wb') as f:
                f.write('')
        readline.read_history_file(hist)
        self.do_trace('off')

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

    def do_changed(self, _):
        if not interpreter.changed:
            print 'nothing changed.'
            print
            return
        print
        print 'Changed'
        print '============='
        for x, v in interpreter.changed.items():
            print x, ':=', v
        print

    def do_chart(self, args):
        if not args:
            interpreter.dump_charts()
        else:
            unrecognized = set(args.split()) - set(interpreter.chart.keys())
            for f in unrecognized:
                print 'unrecognized predicate', f
            if unrecognized:
                print 'available:\n\t' + '\t'.join(interpreter.chart.keys())
                return
            for f in args.split():
                print interpreter.chart[f]
                print

    def emptyline(self):
        """Do nothing on empty input line"""
        pass

    def do_ip(self, _):
        interpreter.ip()

    def do_go(self, _):
        interpreter.go()

    def do_trace(self, args):
        if args == 'on':
            interpreter.trace = sys.stdout
        elif args == 'off':
            interpreter.trace = file(os.devnull, 'w')
        else:
            print 'Did not understand argument %r please use (on or off).' % args

    def do_debug(self, line):
        interpreter.dynac_code(line, debug=True, run=False)

    def do_query(self, line):

        if line.endswith('.'):
            print "Queries don't end with a dot."
            return

        query = 'zzz set= _VALUE is %s, eval("print 12345"), _VALUE.' % line

        print blue % query

        self.default(query)

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
            if interpreter.dynac_code(line):  # failure.
                return
        except interpreter.AggregatorConflict as e:
            print 'AggregatorConflict:', e
        else:
            self.do_changed('')

    def cmdloop(self, _=None):
        try:
            super(REPL, self).cmdloop()
        except KeyboardInterrupt:
            print '^C'
            self.cmdloop()


def repl(hist):
    REPL(hist).cmdloop()

