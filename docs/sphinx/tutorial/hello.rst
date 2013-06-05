.. Hello World tutorial page
   This file is enumerated in the toctree of tutorial/index.rst

.. index::
   single: Tutorial; Hello World

Hello World
***********

Welcome to the Dyna tutorial!

It is traditional to start by writing and running a program that prints
hello world.  :ref:`Downlad Dyna </download>` and follow the instructions in
``README.md`` to build it.  Then, look at the file
``examples/helloworld.dyna`` (or :dynasrc:`here <examples/helloworld.dyna>`).
It should contain::

  goal += hello*world.        % an inference rule for deriving values
  hello := 6.                 % some initial values
  world := 7.

This does not print hello world. It was the closest we could come. Dyna is a
*pure* language. It focuses on computation, and sniffs haughtily at mundane
concerns like input and output. 

Running Hello World
===================

After building Dyna, you may ask our interpreter to run ``helloworld`` by
executing ::

  ./dyna examples/helloworld.dyna

At this point, you should see::

  Charts
  ============
  goal/0
  =================
  goal                           := 42
  
  hello/0
  =================
  hello                          := 6
  
  world/0
  =================
  world                          := 7
  
What has happened?  Dyna has compiled and executed the program requested and
printed out its conclusions.  Notably, the item ``goal`` is seen to have
value ``42``.  Whenever the runtime prints all of its conclusions, they are
organized by :term:`functor`

The Interactive Interpreter
===========================

Dyna also comes with an :ref:`interactive interpreter </spec/repl>`.  This
mode allows you to

* append new rules to the program and observe the consequences
* make custom queries of the conclusions
* visualize the information flow within the program

To run a program interactively, add ``-i`` to the ``dyna`` command line::

  ./dyna -i examples/helloworld.dyna

In addition to the chart printout above, you will be greeted with the
interpreter's prompt, ``:-``.  Interactive help is available by typing
``help`` at the prompt.

Let's try adding a new rule to the program.  Suppose that our goal is not
merely to multiply ``hello`` by ``world`` but to additionally square
``hello``.  At the prompt, type::

  goal += hello**2.

The interpreter will respond with::

  goal := 78

Here you can see that ``goal``'s value has changed to be ``78``.  But wait,
is that right?  We can check by typing at the prompt::

  query hello**2

.. admonition:: bug

   The output for the query is not especially friendly.  There's a
   :githubbug:`bug <1>` filed about that and it's being worked on.

If we modify one of the inputs ``hello`` or ``world``, by typing::

  hello += 1.

The interpreter will respond with::

  goal := 120
  hello := 8
  out(3) := [(64, {})]
 
So not only is it telling us that ``hello`` has changed, and that ``goal``
now takes on a new value as a result, but it reminds us that the query we
ran earlier also has a new value.

At this point, we invite you to continue the tutorial by :doc:`finding the
shortest path <dijkstra>`.
