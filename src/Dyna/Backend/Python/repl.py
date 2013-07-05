# -*- coding: utf-8 -*-

"""
TODO: unsubscribe

TODO: subscriptions probably should only show "changes"

TODO: help should print call signature of loads and post-processors in addition
to help.
"""

import os, cmd, readline
from utils import dynac, ip, lexer, subst, drepr, _repr, get_module
from stdlib import topython
from errors import DynaCompilerError, DynaInitializerException
from config import dotdynadir
from errors import show_traceback
from interpreter import Interpreter, foo, none
import load, post


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
        return '> '

    def do_rules(self, _):
        """
        List rules in the program.
        """
        self.interp.dump_rules()

    def do_retract_rule(self, idx):
        """
        Retract rule from program by rule index.

          > a += 1.
          > b += 1.
          > c += a*b.

        In order to retract a rule we need to know it's index, for that we use
        the command `rules`.

          > rules

          Rules
          =====
            0: a += 1.
            1: b += 1.
            2: c += a * b.

        Now let's remove a rule:

          > retract_rule 0

        This removes rule 0 from the program. Now, let's inspect the changes to
        the solution.

          > sol

          Solution
          ========
          b = 1.

        """

        try:
            idx = int(idx)
        except ValueError:
            print 'Please specify an integer. Type `help retract_rule` to read more.'
        else:
            changes = self.interp.retract_rule(idx)
            if changes is None:
                print 'List available by typing `rules`'
                print
            else:
                self._changed(changes)

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

#    def do_ip(self, _):
#        """
#        Development tool. Jump into an interactive python shell.
#        """
#        ip()

#    def do_debug(self, line):
#        """
#        Development tool. Used for view Dyna's intermediate representations.
#        """
#        import debug
#        with file(dotdynadir / 'repl-debug-line.dyna', 'wb') as f:
#            f.write(line)
#        debug.main(f.name)

#    def do_run(self, filename):
#        """
#        Load dyna rules from `filename`.
#
#        > run examples/papa.dyna
#
#        """
#        try:
#            changed = self.interp.do(self.interp.dynac(filename))
#        except DynaCompilerError as e:
#            print e
#        else:
#            self._changed(changed)

    def _query(self, q):

        if not q.strip():
            print 'No query specified. Type `help query` for usage.'
            return

        if q.endswith('.'):
            print "Queries don't end with a dot."
            return

        self.interp.new_rules = set()

        try:
            query = "$query dict= %s." % q

            self.default(query, show_changed=False)

            try:
                [(_, _, results)] = self.interp.chart['$query/0'][:,]

                return [dict(r) for r in topython(results)]

            except ValueError:
                return []

        finally:

            # cleanup:
            # retract newly added rules.
            for r in self.interp.new_rules:
                if r in self.interp.rules:
                    self.interp.retract_rule(r)

            try:
                # drop $out chart
                del self.interp.chart['$query/0']
            except KeyError:
                # query must have failed.
                pass

        self.interp.new_rules = set()

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
        results = [(b.pop('$val'), b) for b in results]
        for val, b in sorted(results):
            print _repr(val), 'where', drepr(b)
        print

    def do_query(self, q):
        """
        Query solution.

        Consider the following example;

          > f(1) := 1.
          > f(2) := 4.

        There a few versions of query:

         - `vquery` shows variable bindings

            > vquery f(X)
            1 where {X=1}
            4 where {X=1}

         - `query` shows variable bindings applied to query

            > query f(X)
            f(1) = 1.
            f(2) = 4.

         - `trace` is an introspection tool for visualizing the derivation of an
           item and its value. Type `help trace` for more information.

        """
        results = self._query(q)
        if results is None:
            return
        if len(results) == 0:
            print 'No results.'
            return
        print
        for term, result in sorted((subst(q, result), result) for result in results):
            print '%s = %s.' % (term, _repr(result['$val']))
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
            print 'new rule(s) were not added to program.'
            print
        else:
            if show_changed:
                self._changed(changed)

    def _changed(self, changed):
        if not changed:
            return

        changed = [x for x in changed if not x.fn.startswith('$rule/')]

        if not changed:
            return

        print
        print 'Changes'
        print '======='
        for x in sorted(changed):
            print '%s = %s.' % (x, _repr(x.value))
        print

#    def _changed_subscriptions(self, changed):
#
#        # TODO: this doesn't show changes - it redumps everything.
#
#        if not changed:
#            return
#        for x, _ in sorted(changed.items()):
#            if x.fn == '$subscribed/2':
#                [i, q] = x.args
#                if x.value:
#                    print '%s: %s' % (i, q)
#                    for result in x.value:
#                        print ' ', _repr(result.value), 'where', drepr(dict(result.variables))
#        print
#        self.interp.dump_errors()

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

#    def do_subscribe(self, line):
#        """
#        Establish a subscription to the results of a query.
#
#        For example,
#
#            > subscribe f(X,X)
#            > f(1,1) := 1. f(1,2) := 2. f(2,2) := 3.
#            Changes
#            =======
#            f(X,X):
#                1 where {X=1}
#
#        To view all subscriptions:
#
#            > subscriptions
#            f(X):
#               1 where {X=1}
#               2 where {X=2}
#
#        """
#        if line.endswith('.'):
#            print "Queries don't end with a dot."
#            return
#        # subscriptions are maintained via forward chaining.
#        query = '$subscribed(%s, %s) dict= %s.' % (self.lineno, _repr(line), line)
#        self.default(query)
#
#    def do_subscriptions(self, _):
#        "List subscriptions. See subscribe."
#        for (_, [_, q], results) in self.interp.chart['$subscribed/2'][:,:,:]:
#            if results:
#                print q
#                for result in results:
#                    print ' ', _repr(result.value), 'where', drepr(dict(result.variables))
#        print

    def do_help(self, line):
        mod = line.split()
        if len(mod) <= 1:
            return super(REPL, self).do_help(line)
        else:
            if len(mod) == 2:
                [cmd, sub] = mod
                if cmd in ('load', 'post'):
                    try:
                        m = get_module(cmd, sub)
                        print m.__doc__
                    except (ImportError, KeyError, AttributeError):
                        print 'No help available for "%s %s"' % (cmd, sub)
                        return
                    else:
                        return
        print 'Error: Did not understand help command.'

    def do_load(self, line):
        """
        Execute load command.

        Available loaders:

            {load}

        For more information about a particular loader type the following (in
        this case we get help for the `tsv` loader):

            > help load tsv

        Examples:

        > load data = tsv("examples/data/data.csv", delim=',')
        > sol
        Solution
        ========
        data/3
        ======
        data(2,"cow","boy") = true.

        data/4
        ======
        data(0,"a","b","3.0") = true.
        data(1,"c","d","4.0") = true.

        """
        try:
            changed = load.run(self.interp, line)
        except:
            show_traceback()
            readline.write_history_file(self.hist)
        else:
            self._changed(changed)

    def do_post(self, line):
        """
        Execute post-processor.

        Available post-processors:

            {post}

        For more information about a particular post processor (in this case
        `save`)

            > help post save

        """
        try:
            changed = post.run(self.interp, line)
        except:
            show_traceback()
            readline.write_history_file(self.hist)
        else:
            self._changed(changed)

    def do_trace(self, q):
        """
        Trace helps you answer the question "why does the item have this
        value". It is a debugging and introspection tool. The easiest way to
        understand what trace does is by example:

        We'll start with something very simple:

            > a :- b.
            > b :- c.
            > c.

        In our solution we see that `a` is true.

            > sol
            a = true.
            b = true.
            c = true.

        Now we want to find out why

            > trace a

            a = true
            |
            └─ :- true

               a :- b=true.
               |
               └─ b = true
                  |
                  └─ :- true

                     b :- c=true.
                     |
                     └─ c = true
                        |
                        └─ |= true

                           c |= &true.


        The trace shows which rules fired and contributed to the value we
        eventually get for `a`.


        Trace does a few interesting things when substructure is present or
        programs involve cyclic computation.


        Consider the following dyna program:

          % shared substructure
          :- backchain foo/1.
          :- backchain bar/2.
          foo(X) = X+1.
          bar(A,B) += foo(A)*B.
          bar(A,B) += A*foo(B).

          % geometric series
          a += 1.
          a += a/2.

        Now if we run the example interactively and inspect our results with
        trace we see a that rather than print in an infinite loop or print the
        same derivation over and over, only prints one derivation.

        The way trace lets you know that it has omitted something is with a
        message `item: shared structure see above` or `item: *cycle*`.

          > trace bar(10, 10)

          bar(10,10) = 220
          |
          ├─ += 110
          │
          │  bar(A=10, B=10) += (foo(A=10)=11 * B=10)=110.
          │  |
          │  └─ foo(10) = 11
          │     |
          │     └─ = 11
          │
          │        foo(X=10) = (X=10 + 1)=11.
          │
          │
          └─ += 110

             bar(A=10, B=10) += (A=10 * foo(B=10)=11)=110.
             |
             └─ foo(10) = 11
                |
                └─ continue as before (shared structure)


        Now, let's have a look at the geometric series, `a`.

          > trace a

          a = 2.0
          |
          ├─ += 1
          │
          │  a += 1.
          │
          └─ += 1.0

             a += (a=2.0 / 2)=1.0.
             |
             └─ a = 2.0
                |
                └─ continue as before (cyclic structure, will continue forever)

        """

        if not q.strip():
            print 'No query specified. Type `help trace` for usage information.'
            return

        if q.endswith('.'):
            print "Queries don't end with a dot."
            return

        self.interp.new_rules = set()

        try:
            query = "$trace dict= _ is %s, &(%s)." % (q,q)

            self.default(query, show_changed=False)

            try:
                [(_, _, results)] = self.interp.chart['$trace/0'][:,]

                results = topython(results)
                results = [dict(r)['$val'] for r in results]

            except ValueError:
                print 'no items matching `%s`.' % q
                return

            from post.trace import Tracer
            tracer = Tracer(self.interp)
            for item in results:
                print
                tracer(item)

        finally:
            # cleanup:
            # retract newly added rules.
            for r in self.interp.new_rules:
                if r in self.interp.rules:
                    self.interp.retract_rule(r)

            try:
                # drop $out chart
                del self.interp.chart['$trace/0']
            except KeyError:
                # query must have failed.
                pass

        self.interp.new_rules = set()


    do_load.__doc__ = do_load.__doc__.format(load=', '.join(load.available))
    do_post.__doc__ = do_post.__doc__.format(post=', '.join(post.available))
