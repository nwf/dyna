.. Tutorial chapter on errors
   This file is enumerated in the toctree directive of /tutorial/index.rst

.. index::
   double: tutorial; errors

When Things Go Wrong
####################

Impossible Requests
===================

What happens if a Dyna program attempts to divide by zero, as in::

  a += 1 / b.
  b += 0.

If this is the entirety of the program and no changes are forthcoming
(*e.g.*, we are not in interactive mode) then the semantics of this program
include division by zero, and so must be an error.  What happens when we
attempt to run it?  Our interpreter produces a chart with an annotation::

  Charts
  ============
  a/0
  =================
  
  
  b/0
  =================
  b                              := 0
  
  
  Errors
  ============
  because b is 0:
      division by zero
          in rule test.dyna:4:1-test.dyna:4:12
              a += 1 / b.
  
This last ``Errors`` display indicates that the answers available in the
``Charts`` section is not reliable.

.. caution:: Any error is potentially global!  While it might be possible
   for some programs to more accurately track errors, currently our
   implementation does not.  The net effect of this is that if ever the
   interpreter produces an ``Errors`` section, then the entire chart must be
   considered suspect.

If we run the interactive interpreter and add the rule ``b += 1.``, the
error condition has cleared as it should.  If we then add ``b += -1.``, it
will return.

Non-Termination
===============

Productive Nontermination
-------------------------

As mentioned before, Dyna2 currently uses *agenda-driven semi-naive forward
chaining* for its reasoning.  This algorithm has several excellent
theoretical properties, but suffers from a potentially show-stopping
problem: *it might not stop*.

A Dyna program which includes a definition of the Fibonacci numbers (*e.g.*,
``examples/fib.dyna``) ::

  fib(1) += 1.
  fib(2) += 1.
  fib(X) += fib(X-1) + fib(X-2).

will compile and be accepted by the interpreter, but will attempt to prove
a ``fib`` item for every positive natural number!  Clearly, this task is
going to take a while.

If your program *does* go away for longer than you expect, it is entirely
possible that it is caught in such an infinite loop.  In that case, you may
send it a ``SIGINT`` by hitting Control-C.  The interpreter will then print
out the chart as far as it had determined it.  If this is far bigger than
expected, your program probably has a productive infinite loop.

Fixing The Fib Example
``````````````````````

One way out of this problem is to impose a limit on the program, by writing
instead something like::

  f(X) += f(X-1) + f(X-2) for X < lim.
  lim := 10.

This will limit the system to proving the first ``lim`` Fibonacci numbers.
Of course, that can expand or contract as you define ``lim``.

.. index::
   single: counting to infinity

Counting To Infinity
--------------------

Unfortunately, another kind of nontermination error can arise in cyclic
programs, which is not so easy to fix: the so-called *count-to-infinity*
problem.

If we were to have ``examples/dijkstra.dyna`` loaded in the interpreter and
then run ::

  :- start := "NoSuch".

Where there is no such ``"NoSuch`` vertex, the interpreter will appear to be
pondering this change to the universe for "a while", as we say.  If we
interrupt it (with Control-C) after a while, the chart will contain, among
other things::

  path/1
  =================
  path("DFW")                    := 10124432
  path("LAX")                    := 10124063
  path("MyHouse")                := 10122046
  path("NoSuch")                 := 0
  path("ORD")                    := 10123630
  path("SFO")                    := 2779

This arises from the fact that our graph contains a cycle::

  edge("DFW","ORD") := 802.
  edge("ORD","DFW") := 802.
  edge("LAX","ORD") := 1749.

Note that it is also possible to "count to infinity" in other directions,
such as by counting down to :math:`-\infty` or by approaching a finite
solution but as in Zeno's paradox.

.. admonition:: bug
   
   There is, as of yet, no good solution to this problem; the best
   work-around might just be to start the program over.
