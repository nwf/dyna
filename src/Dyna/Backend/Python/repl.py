"""
TODO: unsubscribe

TODO: should probably remove the new rule after we get the results.

TODO: subscriptions probably should only show "changes"

TODO: queries are all maintained... should probably toss out the query rule. If
users want queries to be kept up-to-date user should subscribe instead.

TODO: help should print call signature of loads and post-processors in addition
to help.

TODO: $include load rules from a file.
"""

import re, os, cmd, readline

import debug, interpreter
from utils import ip, lexer, subst
from errors import DynaCompilerError, DynaInitializerException
from chart import _repr
from config import dotdynadir

from errors import show_traceback
import load, post

from interpreter import Interpreter, foo, none

from term import _repr
from defn import drepr


class REPL(cmd.Cmd, object):

    def __init__(self, interp, hist=dotdynadir / 'dyna.hist'):
        self.interp = interp
        cmd.Cmd.__init__(self)
        self.hist = hist
        if not os.path.exists(hist):
            readline.clear_history()
            with file(hist, 'wb') as f:
                f.write('')
        readline.read_history_file(hist)
        self.lineno = 0

        # create help routines based on doc string.
        for x, v in REPL.__dict__.iteritems():
            if x.startswith('do_') and hasattr(v, '__doc__'):
                def show_doc(d=v.__doc__):
                    print d
                setattr(self, 'help_' + x[3:], show_doc)

    @property
    def prompt(self):
        return ':- '

    def do_rules(self, _):
        """
        List rules in the program.
        """
        self.interp.dump_rules()

    def do_retract_rule(self, idx):
        """
        Retract rule from program by rule index.

        :- a += 1.
        :- b += 1.
        :- c += a*b.

        :- rules
          0: a += 1.
          1: b += 1.
          2: c += a * b.

        :- retract_rule 0

        This removes rule 0 from the program. Now, let's inspect the changes to
        the solution.

        :- sol

        Solution
        ========
        b := 1.

        """
        self.interp.retract_rule(int(idx))

    def do_exit(self, _):
        """
        Exit REPL by typing exit or control-d. See also EOF.
        """
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

    def do_sol(self, _):
        """
        Show solution.
        """
        self.interp.dump_charts()

    def emptyline(self):
        """Do nothing on empty input line"""
        pass

    def do_ip(self, _):
        """
        Development tool. Jump into an interactive python shell.
        """
        ip()

    def do_debug(self, line):
        """
        Development tool. Used for view Dyna's intermediate representations.
        """
        with file(dotdynadir / 'repl-debug-line.dyna', 'wb') as f:
            f.write(line)
        debug.main(f.name)

    def _query(self, q):
        if q.endswith('.'):
            print "Queries don't end with a dot."
            return
        query = '$out(%s) dict= %s.' % (self.lineno, q)
        self.default(query, show_changed=False)
        try:
            [(_, _, results)] = self.interp.chart['$out/1'][self.lineno,:]
        except ValueError:
            return []
        return results

    def do_vquery(self, q):
        """
        See query.
        """
        results = self._query(q)
        if results is None:
            return
        if len(results) == 0:
            print 'No results.'
            return
        for val, bindings in results:
            print '   ', _repr(val), 'when', drepr(dict(bindings))
        print

    def do_query(self, q):
        """
        Query solution.

        Consider the following example;

          :- f(1) := 1.
          :- f(2) := 4.

        There a few versions of query:

         - `vquery` shows variable bindings

            :- vquery f(X)
                1 when {X=1}
                4 when {X=1}

         - `query` shows variable bindings applied to query

            :- query f(X)
                1 is f(1)
                4 is f(2)

        """
        results = self._query(q)
        if results is None:
            return
        if len(results) == 0:
            print 'No results.'
            return
        for term, result in sorted((subst(q, dict(result.variables)), result) for result in results):
            print '   ', _repr(result.value), 'is', term
        print

    def default(self, line, show_changed=True):
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
            changed = self.interp.do(src)

        except (DynaInitializerException, DynaCompilerError) as e:
            print type(e).__name__ + ':'
            print e
            print '> new rule(s) were not added to program.'
            print
        else:
            if show_changed:
                self._changed(changed)

    def _changed(self, changed):
        if not changed:
            return
        print '============='
        for x, v in sorted(changed.items()):
            print '%s := %s' % (x, _repr(v))

    def _changed_subscriptions(self, changed):

        # TODO: this doesn't show changes - it redumps everything.

        if not changed:
            return
        for x, _ in sorted(changed.items()):
            if x.fn == '$subscribed/2':
                [i, q] = x.args
                if x.value:
                    print '%s: %s' % (i, q)
                    for result in x.value:
                        print ' ', _repr(result.value), 'when', drepr(dict(result.variables))
        print
        self.interp.dump_errors()

    def cmdloop(self, _=None):
        try:
            super(REPL, self).cmdloop()
        except KeyboardInterrupt:
            # Catch Control-C and resume REPL.
            print '^C'
            readline.write_history_file(self.hist)
            self.cmdloop()
        finally:
            readline.write_history_file(self.hist)

    def do_subscribe(self, line):
        """
        Establish a subscription to the results of a query.

        For example,

            :- subscribe f(X,X)
            :- f(1,1) := 1. f(1,2) := 2. f(2,2) := 3.
            Changes
            =======
            f(X,X):
                1 when {X=1}

        To view all subscriptions:

            :- subscriptions
            f(X):
               1 when {X=1}
               2 when {X=2}

        """
        if line.endswith('.'):
            print "Queries don't end with a dot."
            return
        # subscriptions are maintained via forward chaining.
        query = '$subscribed(%s, %s) dict= %s.' % (self.lineno, _repr(line), line)
        self.default(query)

    def do_subscriptions(self, _):
        "List subscriptions. See subscribe."
        for (_, [_, q], results) in self.interp.chart['$subscribed/2'][:,:,:]:
            if results:
                print q
                for result in results:
                    print ' ', _repr(result.value), 'when', drepr(dict(result.variables))
        print

    def do_help(self, line):
        mod = line.split()
        if len(mod) <= 1:
            return super(REPL, self).do_help(line)
        else:
            if len(mod) == 2:
                [cmd, sub] = mod
                if cmd in ('load', 'post'):
                    try:
                        print getattr(globals()[cmd], sub).__doc__
                    except (KeyError, AttributeError):
                        print 'No help available for "%s %s"' % (cmd, sub)
                        return
                    else:
                        return
        print 'Error: Did not understand help command.'

    def do_load(self, line):
        """
        Execute load command.

        Available loaders:

            {loaders}

        For more information about a particular loader type the following (in
        this case we get help for the `tsv` loader):

            :- help load tsv

        Examples:

        :- load data = tsv("examples/data/data.csv", delim=',')
        :- sol
        Solution
        ========
        data/3
        ======
        data(2,"cow","boy")            := true

        data/4
        ======
        data(0,"a","b","3.0")          := true
        data(1,"c","d","4.0")          := true

        """
        try:
            load.run(self.interp, line)
        except:
            show_traceback()
            readline.write_history_file(self.hist)

    do_load.__doc__ = do_load.__doc__.format(loaders=', '.join(x for x in dir(load) if not x.startswith('_')))

    def do_post(self, line):
        """
        Execute post-processor.

        Available post-processors:

            {post}

        For more information about a particular post processor (in this case
        `save`)

            :- help post save

        """
        try:
            post.run(self.interp, line)
        except:
            show_traceback()
            readline.write_history_file(self.hist)

    do_post.__doc__ = do_post.__doc__.format(post=', '.join(x for x in dir(post) if not x.startswith('_')))
