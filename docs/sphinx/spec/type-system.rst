.. -*- compile-command: "make -C .. html" -*-
.. Type system specification

.. todo:: This really is just an outline.


####################
  Dyna Type System
####################

Dyna uses a ``set-theoretic`` type system: abstractly, a type is simply a
set of ground terms and all operations of the type system are defined in
terms of operations on sets.  In practice, our system may be instantiated
with any *computable description* of sets closed under the requisite
operations.

Type Roles
----------

In a pure Dyna (one without foreign primops) we could say that *every* term
is an item and that every query is therefore supported, but might fail.
However, efficient use of foreign primops forces us to constrain the
system's actions: some primops are only capable of working with certain
terms (e.g. ``sqrt`` only knows how to operate on numbers, not strings).

Type checking
-------------

Type checking enforces that every term construction and evaluation obeys any
type restrictions (though it does not guarantee executability of a rule --
that falls to the mode system).  That is, we assume that we have an *upper
bounds* on the types of terms and items available in the program, and we
would like to ensure that every rule's implied upper bound is non-empty.
This upper-bound is computed by simply intersecting upper-bounds of each
construction and evaluation in a rule.

However, in addition, our type checking algorithm imposes constraints in an
effort to catch situations which are likely to be unintentional errors.  We
try to model an intuitive understanding of information flow, akin to those
in more traditional programming langues, even though our system can execute
things "out of order."  In this intuitive model (which is not necessarily
indicitative of the evaluation order planned by the compiler), information
flows between sub-expressions in a left-to-right, innermost-out order: in
``f(X), g(X)``, the ``f`` subgoal grounds ``X`` (if it is not already) and
``g`` must be prepared to handle that ground form; the same is true in
``h(f(X),g(X))``.

More formally, when a rule is de-sugared, we preserve the left-to-right DFS
order of sub-expressions.  Any type annotations in the head of the rule also
become sub-expressions and are placed at the front of this order.  Starting
from the assumption that all variables range over all terms, the type
checker then traverses sub-expressions in order, intersecting the current
context with the term and, if evaluated, item types for the subgoal under
consideration.  Only variables *first-mentioned* in this subgoal are
permitted to have a narrower type after intersection; if this check fails,
type checking aborts.  If ever the type context implies an empty set of
results, type checking aborts.

Type declarations
-----------------

.. todo:: old design at http://www.dyna.org/wiki/index.php?title=Term#Union_types

Typed variables
---------------

Type inference
--------------

The other source of constraints on types comes from an inference pass, which
attempts to fill in declarations not given.

Our inference algorithm, like the type checking algorithm, is set-theoretic
and therefore parameterized by a representation of sets.  Inference is
further parametric in a union-semilattice of types, which we will call
"natural" types.  The set of "natural" types is likely to include types like

* Numeric families, like ``:int`` and ``:float``, and other primitive
  families like ``:string``.

* The Herbrand universe of Dyna functors over natural types, each as an
  element of the lattice.

* The set of all ground terms.

Type inference proceeds inflationarily, beginning with the assumption that
*only* pre-declared (including primitive) types exist.  It then runs a
variant of type-checking...

.. todo:: 

Co-inductive types
------------------

.. todo:: how do we deal with types for varargs and keyword args?

Possible future extensions
--------------------------
Guarded types?  Nonlinear types?  Parametric types?

Type coercion
=============


