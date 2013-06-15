.. Builtins
   Here we describe the primitives available by default.

********
Builtins
********

Aggregators
===========

For aggregation, we offer

* Numerics: ``max=``, ``min=``, ``+=`` (:math:`\sum`), ``*=`` (:math:`\prod`)
* Logic: ``&=`` (:math:`\bigwedge`), ``|=`` (:math:`\bigvee`).

* A last-one-wins operation, ``:=``.  Formally, the last rule which
  contributes a value determines the head item's value.  That is, a program
  such as ::

    a := 1.
    a := 2 for d.

  will give ``a`` the value of ``1`` if ``d`` is not provable or is not
  ``true`` and ``2`` otherwise.

Functions
=========

The following list of functions are guaranteed to be present, regardless of
backend chosen:

* The usual binary numeric operations: ``*``, ``-``, ``*``, ``/``,
  ``mod`` (or ``%``), and ``**`` (for raising to a power).

* Some unary numeric operations: ``-``, ``abs``, ``log``, and ``exp``.

* Comparison operators: ``<``, ``<=``, ``==``, ``>=``, ``>``, and ``!=``
  (disequality).

* Logic operations: ``and`` (or ``&``), ``or`` (or ``|``), ``^``
  (for exclusive or), and ``not`` (or ``!``).

* Unification is written ``=``.  Prolog's ``is`` operator is also available.

.. warning::

   The distinction between ``=`` and ``==`` is that the latter *evaluates
   both of its arguments* while the former does not.  Meanwhile, ``is``
   is asymmetric, evaluating its right argument and not its left.

   See ``examples/equalities.dyna``
   (or :dynasrc:`here <examples/equalities.dyna>`).

Constants
=========

Integers, floats, and double-quoted strings all exist as primtives in the
language.  Booleans are represented by the atoms ``true`` and ``false``.
