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

This last ``Errors`` display indicates that the answers available in the
``Charts`` section is not reliable.

.. caution:: Any error is potentially global!  While it might be possible
   for some programs to more accurately track errors, currently our
   implementation does not.  The net effect of this is that if ever the
   interpreter produces an ``Errors`` section, then the entire chart must be
   considered suspect.

Non-Termination
===============

As mentioned before, Dyna2 currently uses *agenda-driven semi-naive forward
chaining* for its reasoning.  This algorithm has several excellent
theoretical properties, but suffers from a potentially show-stopping
problem: *it might not stop*.

A Dyna program which includes a definition of the Fibonacci numbers ::

  fib(1) += 1.
  fib(2) += 1.
  fib(X) += fib(X-1) + fib(X-2).

will compile and be accepted by the interpreter, but will attempt to prove
a ``fib`` item for every positive natural number!  Clearly, this task is
going to take a while.

If your program goes away for longer than you expect, it is entirely
possible that it is caught in such an infinite loop.  In that case, you may
send it a ``SIGINT`` by hitting Control-C.  The interpreter will then print
out the chart as far as it had determined it.  If this is far bigger than
expected, your program probably has a productive infinite loop.
