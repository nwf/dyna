.. Pragmas
   This file documents the pragma assertions our pipeline understands.

*******
Pragmas
*******

Pragmas are used to pass a wide variety of information in to the system.
They are visually separated by begining with ``:-``.

######
Syntax
######

Some pragmas alter the syntax of the language.

.. index::
   double: pragma; disposition

.. _pragma_disposition:

Disposition
===========

In Dyna source code, there are two different things that the term ``f(1,2)``
could mean:

* Construct the piece of data whose functor is ``f`` and has arguments
  ``1`` and ``2``, as in ``f(A,B) = f(1,2)``, which unifies ``A`` with ``1``
  and ``B`` with ``2``.

* Compute the value of the ``f(1,2)`` item, as in  ``f(1,2) + 3`` or
  ``Y is f(1,2)``.

It is always possible to explicitly specify which meaning to use, by use of
the ``&`` and ``*`` operators (see :ref:`syntax-quote-eval`), but this would
be tedious if it were the only solution.  As such, we endow functors (of
given arity) with *dispositions*, which indicate, by default, how they would
like to treat their arguments.

Dispositions are specified with the ``:-dispos`` pragma, thus::

  :-dispos g(&).      % g quotes its argument.
  :-dispos '+'(*,*).  % + evaluates both arguments.

Now ``g(f(1,2)) + 1`` will pass the structure ``f(1,2)`` to the ``g``
function and add ``1`` to the result.  Note that dispositions take effect
*while the program is being parsed*.  That is, a program like::

  :-dispos f(&).
  goal += f(g(1)).
  :-dispos f(*).
  goal += f(g(2)).

specifies that ``goal`` has two antecedents: the ``f`` images of ``g(1)``
and the ``g`` image of ``2``.

It is also possible to indicate that some terms should not be evaluated::

  :-dispos &pair(*,*).  % pair suppresses its own evaluation

In the case of disagreements, like ``pair(1,2) + pair(3,4)``, the preference
of the argument is honored.

.. admonition:: Defaults

   Absent any declarations, all functors are predisposed to evaluate their
   arguments.  Some functors (``pair/2``, ``true/0``, and ``false/0``)
   suppress their own evaluation.

More Detail
-----------

.. warning:: This section is probably relevant only if you are a developer
   of the Dyna compiler.

Requesting Evaluation
^^^^^^^^^^^^^^^^^^^^^

Just like it is possible to request that some functors not be evaluated even
when in evaluation context, it is additionally possible for functors to
request that they be evaluated even when the context is one of quotation::

  :-dispos *f(*).

The neutral position of specifying neither ``&`` nor ``*`` before a pragma
is termed *inherit*, which means that the context or overrides apply.  Under
the defaults above, this is the default position for all functors.

Disposition Defaults
^^^^^^^^^^^^^^^^^^^^

It is possible to override the defaults, as well; at least one of us has a
stylistic preference for a more Prolog-styled structure-centric view of the
universe.  The pragma::

  :-dispos_def prologish.

will cause subsequent rules to behave as if all functors which start with an
alphanumeric character had had ``:-dispos f(&,...,&)`` asserted, while all
other functors had had ``:-dispos *f(*,...,*)``.  There are, however, a few
built-in overrides to this rule of thumb, giving alphabetic mathematical
operators (*e.g.* ``abs``, ``exp``, ...) their functional meaning.  See
:dynasrc:`src/Dyna/Term/SurfaceSyntax.hs`

The default default rules may be brought back in by either::

  :-dispos_def dyna.
  :-dispos_def.

Note that when chaning defaults, any manually-speficied ``:-dispos``
pragmas remain in effect.

.. index::
   double: pragma; operator

.. _pragma_operator:
  
Operators
=========
  
Dyna aims to have a rather flexible surface syntax; part of that goal is
achieved by allowing the user to specify their own operators.

As with :ref:`pragma_disposition`, these pragmas take effect
*while the program is being parsed*.

.. admonition:: bug

   The ability to add and remove operators is not yet actually supported.

Adding an operator
------------------

The ``:-oper add`` pragma takes three arguments: the fixity, priority, and
lexeme that makes up the operator.  Fixities are specified as ``pre``,
``post`` or ``in``.  Priorities are natural numbers, with higher numbers
binding tighter.  Lexemes are either bare words or singly-quoted functors.

Examples::

  :-oper add in 6 + .
  :-oper add pre 9 - .

Removing an operator
--------------------

The ``:-oper del`` pragma may be used to remove all previously added forms
of a given operator.

Defaults
--------

The default operator table is, hopefully, more or less what you might
expect and follows the usual rules of arithmetic.

.. admonition:: bug

   For the moment, the source is the spec.  See the source in
   :dynasrc:`src/Dyna/Term/SurfaceSyntax.hs` for full details.

#########
Execution
#########

On the other hand, some pragmas impact the execution of the system.

.. index::
   double: pragma; inst
   double: pragma; mode

.. _pragma_inst_mode:

Insts and Modes
===============

Following the [MercuryLang]_ syntax, we allow the user to give names to
instantiation states and modes::

  :-inst name(args) == ... .
  :-inst mode(args) == ... >> ... .

.. index::
   double: pragma; query mode
   single: qmode

.. _pragma_qmode:

Query Modes
===========

A Query mode specifies that a particular backward-chaining operation is to
be available to the system.  These capture the change in instantiation
state, determinism, and other properties of a query.
