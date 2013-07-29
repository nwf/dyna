.. -*- compile-command: "make -C .. html" -*-g
.. Dijkstra tutorial page
   This file is enumerated in the toctree of tutorial/index.rst

.. index::
   double: Shortest Path; Tutorial

Shortest Path in a Graph
************************

We hope that Dyna offers the shortest ever shortest path program::

  path(start) min= 0.
  path(B) min= path(A) + edge(A,B).
  goal min= path(end).

This program already highlights one of the features of Dyna: the first rule
and last rules are *dynamic*: the *value* of the ``start`` item determines
which vertex in the graph is used as the start, and similarly the value of
``end`` is used to select which vertex matters to ``goal``.

This program is available in ``examples/dijkstra.dyna`` (or
:dynasrc:`here <examples/dijkstra.dyna>`).

Encoding the Input
==================

The following input graph is adapted from Goodrich & Tamassia's data
structures textbook. It shows several available flights between U.S.
airports, with their distances in miles. We would like to get from a
friend's house, 10 miles from Boston (BOS), to our destination, 20 miles
from Chicago (ORD).

.. graphviz::

  digraph foo {
    graph[size="7,2",rankdir="LR"]

    BOS   -> JFK [label=187]
    BOS   -> MIA [label=1258]
    JFK   -> DFW [label=1391]
    JFK   -> SFO [label=2582]
    JFK   -> MIA [label=1090]
    MIA   -> DFW [label=1121]
    MIA   -> LAX [label=2342]
    DFW   -> ORD [label=802]
    DFW   -> LAX [label=1235]
    ORD   -> DFW [label=802]
    LAX   -> ORD [label=1749]

    FriendHouse -> BOS [label=10]
    ORD     -> MyHouse [label=20]
  }

.. sidebar:: Shortest Paths

   If we work things out by hand (or just ask Dyna) we will discover
   that the shortest path to each node from "FriendHouse" is

   .. rst-class:: center
   ..

     ============= =====
      Destination  Total
     ============= =====
     FriendHouse   0
     BOS           10
     JFK           197
     MIA           1268
     DFW           1588
     ORD           2390
     MyHouse       2410
     SFO           2779
     LAX           2823
     ============= =====

This is encoded into Dyna, using strings to identify vertices of the graph,
thus::

  edge("BOS","JFK") := 187.
  edge("BOS","MIA") := 1258.
  edge("JFK","DFW") := 1391.
  edge("JFK","SFO") := 2582.
  edge("JFK","MIA") := 1090.
  edge("MIA","DFW") := 1121.
  edge("MIA","LAX") := 2342.
  edge("DFW","ORD") := 802.
  edge("DFW","LAX") := 1235.
  edge("ORD","DFW") := 802.
  edge("LAX","ORD") := 1749.
  
  edge("FriendHouse","BOS") := 10.
  edge("ORD","MyHouse") := 20.

``edge`` pairs that are not specified are said to be :term:`null`; that is,
they have no value, and can be thought of as the identity of the aggregator
``min=``, or :math:`+\infty`, meaning "You can't get there directly from
here."

And of course, we need to specify whence we come and where it is we would
like to end up::

  start := "FriendHouse".
  end   := "MyHouse".

Run the program
===============

We can run this program in the interpreter::

  ./dyna -i examples/dijkstra.dyna

We are met with the conclusions, which include all the data we fed in as
well as a pile of ``path`` assertions.  Of course, that's not so useful,
necessarily, so let's just ask for the answer::

  :- query goal
  out(0) := [(2410, {})]

As we can see, the total weight of the shortest path is ``2410``.  What
happens, though, if we realize that we will be by the airport anyway? ::

  :- start := "BOS".
  =============
  goal := 2400
  out(0) := [(2400, {})]
  path('BOS') := 0
  path('DFW') := 1578
  path('FriendHouse') := None
  path('JFK') := 187
  path('LAX') := 2813
  path('MIA') := 1258
  path('MyHouse') := 2400
  path('ORD') := 2380
  path('SFO') := 2769
  start := 'BOS'

And just like that, the total path weight from ``start`` to ``end`` is now
``2400``.  The system also tells us a number of potentially interesting
things:

* The system has in fact computed the revised ``path`` costs to each other
  vertex.

* There is no path from ``"BOS"`` to ``"FriendHouse"`` (thus ``None``).

* A query we had made earlier has changed its answer.

Explaining Answers
==================

.. admonition:: bug

   We do not yet have a good mechanism implemented, though it's just a
   matter of time.  See :githubbug:`1`.

Understanding The Program
=========================

Simply stated, this program looks for all paths from the vertex indicated by
``start``.  Formally, the technique currently used is called *agenda-driven
semi-naive forward chaining* [#snfc]_ .

Inference Rules
---------------

The first inference rule states that there is no distance on the degenerate
path that does not go anywhere.::

  path(start) min= 0.

Alternatively, there is a path to vertex ``B`` if there is a path to some
vertex ``A`` such that an edge connects ``A`` to ``B``.::

  path(B) min= path(A) + edge(A,B).

The final rule merely says that we are looking for the best path to the
vertex indicated by ``end``.::

  goal min= path(end).

Inference Rules As Equations
----------------------------

But what are the ``min=`` and ``+`` doing? In fact, the inference rules are
equations. They state how to find the values of all ``pathto`` and ``goal``
items.

Those items have values just like the ``hello``, ``world`` and ``goal``
items in :doc:`the previous example <hello>`. But this program is more
complicated. It involves lots of different ``pathto`` items for different
airports, distinguished from one another by their arguments:
``pathto("JFK")``, ``pathto("MyHouse")``, etc. These items may all have
different values.

Why These Particular Equations?
-------------------------------

Assuming that each ``edge``'s value represents its length in the input graph,
the rules are carefully written so that ``pathto(V)``'s value will be the total
length of the shortest path from the ``start`` vertex to vertex ``V``.

In principle, there are several ways to get to ``V``: one can get there by
an edge from ``start`` or an edge from some other ``U``. The ``min=``
operator finds the minimum over all these possibilities. Think of it as
keeping a running minimum (just as ``+=`` would keep a running total). In
particular, ``pathto(V)`` is found as
:math:`\mbox{min}(\mathtt{edge(start},V), \mbox{min}_U \mathtt{pathto}(U)+\mathtt{edge}(U,V))`
which involves minimizing over all possible ``U``.

If there are no paths to ``V``, then ``pathto(V)`` is a minimum over no
lengths at all.  Dyna specifies that items receiving no inputs take on the
special value :term:`null`, which is the *identity* of every aggregator and
a *zero* of every expression.  Since we aggregate answers with ``min=``,
:term:`null` approximates :math:`+\infty`.

Deriving The Graph From Rules
=============================

There's nothing that mandates that ``edge`` weights be the base case; we
could also derive ``edge`` facts from other facts, such as position and
reachability.  An example is available in ``examples/dijkstra-euclid.dyna``
(or :dynasrc:`here <examples/dijkstra-euclid.dyna>`).


.. todo:: This section is mostly a placeholder!

Endnotes
========

.. [#snfc] There are a multitude of inference algorithms for logic
  programming.  We would like to think that [filardo-eisner-2012]_ provides
  a good overview as well as explaining the basics of what will become
  Dyna 2's inference algorithm.
