.. -*- compile-command: "make -C .. html" -*-
.. Specification index
   This file is enumerated in the toctree directive of index.rst

.. todo:: set up the default role to be code, so that `foo` and
	  ``foo`` are identical.

.. todo::
   Fill in to-do items from various sources, then break this into
   multiple files.  (I believe that in the HTML rendering, each file
   is a webpage.)

   Sources:

   https://mail.google.com/mail/?shva=1#apps/label%3Adyna+OR+label%3Adynasty+OR+label%3Adyna%2Fbugs+OR+label%3Adyna%2Fbuild+OR+label%3Adyna%2Fdistro+OR+label%3Adyna%2Fdynac+OR+label%3Adyna%2Fdynamite+OR+label%3Adyna%2Finference+OR+label%3Adyna%2Fserver+OR+label%3Adyna%2Fstorage+OR+label%3Adyna%2Fsyntax+OR+label%3Adyna%2Ftransforms
   DONE - http://dyna.org
   http://dyna.org/Design and perhaps also http://dyna.org/wiki/index.php?title=Special:LongPages
   http://bugs.dyna.org
      DONE - Bugs
      DONE - Comments
      DONE - Distro
      DONE - Documentation
      DONE - Dynac
      DONE - Dynamite
      Dynasty
      haskell-interpreter
      Help
      Inference
      Misc
      Reading
      Server
      Storage
      Syntax
      Transforms

   http://cs.jhu.edu/~jason/papers/#Dyna
   http://cs.jhu.edu/~darcey/dyna-tutorial.pdf
   ~/grants/*dyna*
   ~/jot/dyna
   ~/jot/dynasty
   ~/jot/dopp
   ~/hw/dyna/docs/inference2.txt
   ~/hw/dyna/docs/manpage.txt
   ~/hw/dyna/docs/manpage-NOTES.txt
   ~/hw/dyna/docs/{papers}  -- includes writeup of dynabase types etc.
   ~/jot/dopp
   ~/hw/dyna/CITE
   ~/hw/dyna/docs/frozen3.txt
   ~/hw/dyna/docs/frozen2.txt
   ~/hw/dyna/docs/frozen.txt
   ~/hw/dyna/docs/frozen-SCRAPS.txt
   ~/hw/dyna/docs/nwf-inference-notes.txt
   ~/jot/dynabases
   ~/hw/dyna/docs/pseudospec.tex
   ~/jot/dynaspec
   ~/Mail/dyna
   ~/hw/dyna/TO-DO-2005
   ~/hw/dyna/SEND-TO


######################################
  Specification of the Dyna Language
######################################

************
Introduction
************

What is Dyna?
=============

.. todo:: keep it brief

.. todo:: Some materials at http://www.dyna.org/ and http://www.dyna.org/wiki/index.php?title=Documentation#Overview.

Intended users
==============

.. todo:: include teaching: http://www.dyna.org/wiki/index.php?title=Teaching

Key features
============

Relation to other work
======================

.. todo:: where to learn more

******************************
How to read this specification
******************************

Organization
============
.. todo:: Reading in order is ok

Notation
========
.. todo:: Unless otherwise noted, we'll spell things using default syntax table.

User comments
=============

Coloring and formatting conventions
===================================

Cross-refs
==========

Sidebars
========

Notifications
=============
.. todo:: deprecation, bugs, future work

Links to examples
=================

Links to issue tracker
======================

Glossary/Index
==============

***************************
Terms (i.e.,  ground terms)
***************************

.. todo:: old design at http://www.dyna.org/wiki/index.php?title=Term

Overview
========
      
Primitive terms
===============

.. todo:: Old design at http://www.dyna.org/wiki/index.php?title=Primitive_%28future_version%29

Booleans
--------

Numbers
-------

.. todo:: One option is to have disjoint numeric types that vary by
	  precision.  There seem to be the following classes: 
	  * exact expressions as in Mathematica (bignums, rationals
	    infinity, algebraic expressions like 1+sqrt(3) or at least
	    the ones without variables, complex numbers with exact
	    coefficients, perhaps unlimited-precision FP)
	  * machine integers of various widths, with modular
	    arithmetic (wraparound); these are not really subtypes
	    of one another because upcasting changes what addition
	    means
	  * machine floating-point of various precisions

          We also have union types like `number` and (to exclude
	  complex numbers from comparisons) `real`.
	  The numeric library is able to add or compare ints and
	  floats with each other: it probably covers numbers via a
	  union of several type signatures.  However, not all modes
	  are supported.  So even though `*` has (int,float)->float,
	  we can only run that in mode +,+,- or +,-,+ but not -,+,+
	  where we need to use (float,float)->float.  And even though
	  `between` has (real,real,real)->bool, the mode +,-,+ 
	  requires the second arg to already be restricted to int.
	  So the user can write `between(1,3.5,10)` but must do
	  `between(1,:int X,10)` rather than `between(1,X,10) (unless
	  the :int restriction shows up elsewhere in the rule).
	  Perhaps we should offer the user a convenience predicate
	  `ibetween`.

	  Problems with this approach:
	  * distinguishing the literals (3 versus 3. versus 3L)
	  * names for explicit casts
	  * operator overloading and/or coercion (borrow from other
	    languages)
	  * queries for f(3) and f(3.0) could give different results
	  * f(3.0) := ... can't override f(3) := ... 
	  * f(3.0) +=  doesn't combine with f(3) += ...; could be
	    awkward if argument to f comes from a subexpression or
	    another query
	  * inverse floating-point modes may have to be nondet if they
	    are supported at all: there are many values of Epsilon
	    such that 3+Epsilon==3 (similarly, inf+Epsilon==inf)
	  * double-counting in aggregation, e.g., += f(X) for X > 3.
	  * Options:
	    * Reject aggregation where the type contains multiple
	      numeric branches.  But this is a pain because now people
	      can't aggregate innocuously over terms when they are not
	      planning to store both 3 and 3.0.
	    * At runtime, give an aggregation error if some of the
	      binding sets that we aggregate over appear to be equal,
	      i.e., X=3,Y="foo" and X=3.0,Y="foo".  (We can
	      eliminate this check if at compile-time we know from X's
	      type that it can't take on different "versions" of the
	      same value, e.g., its int range and float range are
	      disjoint.  We can also speed up this check if at runtime
	      we track information about the numeric types that are
	      actually used and do more type inference based on those
	      restricted types.)
	    * Don't permit the user to define explicit values at
	      compile-time or runtime for both f(3) and f(3.0)
	      (perhaps f needs to keep track of which numeric subclass
	      it is actually using for its args and values even if it
	      is defined over terms), unless user declares that they
	      really want this for f/1.  We probably shouldn't
	      allow the queries f(3) and f(3.0) to find each other's
	      values because then we'll get double-counting.
	 
	  Alternative approach: We only have one big `number` type
	  with subtypes that are implemented by various efficient
	  classes, which may overlap.  This ensures that f(3.0) and
	  f(3) always mean the same thing and are not double-counted.
	  But what happens when a class runs into its bounds or
	  precision limits?  The classy procedures that implement `+`
	  may give different results from one another, depending on
	  the particular classes of their arguments.  

	  (A good rule of thumb is that the most precise argument
	  class dictates the amount of precision in the operation and
	  the result class, and literals have a sufficient and indeed
	  generous level of precision, so 12345678901234567890 will
	  get a class that is big enough to represent it and so that
	  adding 255+1 is adding integers rather than bytes.  However,
	  we need to decide whether bigint+float returns bigint or
	  float or something even bigger, and certain operations may
	  map finite to possibly-infinite or int to float.)

	  Since the class of the input item makes a difference, we
	  think of this nondeterministic behavior as akin to the
	  "don't care" aggregator, where there are several definitions
	  of `+` and which one to use is up to the system.  If the
	  user wants to control which `+` it is (e.g., unsigned char,
	  or strict math that is careful to never lose precision),
	  then they can give a more specifically named operator, or
	  rebind the `+` symbol to that operator for convenience, or
	  give a pragma, or something.

	  A problem with this don't-care approach is that a single
	  item can be computed in different ways (plan order, forward
	  vs. backward chaining, query mode, aggregation order, the
	  subtraction trick, etc.)  that get different answers owing
	  to different class choices or aggregated numerical errors.
	  So we may have to pin a particular answer: remember that
	  don't-cares are as bad for efficiency as cycles or
	  randomness.  So maybe we'd like to ensure at least that the
	  precision of a token of `+` is resolved in a consistent way
	  across all plans (which might require restricting the number
	  of plans).

	  Also, it should be easy for the user to figure out what the
	  semantics of the program is and to control it.  That is, can
	  the user figure out which classes are being used other than
	  by annotating the operator?  Note that `f(X) = X+1` might
	  give different numerical answers for `f(3000000000000)`
	  depending on the class used to represent the argument.
	  Consider also `f(X) = X+1 for a(X), b(X)`, where the choice
	  of `+` procedure might be affected by the classes of ground
	  answers returned by subgoals `a` and `b` as well as the
	  class of ground argument passed in by `f` (if any).  If
	  these classes are not all the same, then what is the class
	  passed into `+`?  Can it be determined somehow at compile
	  time, in a consistent way?  Isn't it a problem if forward
	  and backward chaining give different results?  (Perhaps we
	  can at least say that if the query `f(X)` comes back with
	  the result `f(3)=4` then `f(3)` would have come back with
	  the same answer `4`, and that if it comes back with
	  `f(3.0)=4.0` then `f(3.0)` would have come back with the
	  same answer `4.0`.)

Strings
-------

.. todo:: (related to numbers) when are two Unicode strings considered
	  equal?  Is a string a sequence of code points, or is it a
	  normalized version?  Do we have different types for string,
	  string_NFD, string_NFC, string_NFKD, string_NFKC?  Can they
	  be equal to one another?  Is there any coercion?  What are
	  the canonicalization operators called?  Do we have
	  canonicalizing equality operators?  How do we protect the
	  user?  Should we regard a normalized string in any of these
	  schemes as a *set* of unnormalized strings (probably the
	  enumeration mode is not supported), so that we can ask about
	  meet as well as equality?

Escape codes
^^^^^^^^^^^^
.. todo:: including \' and \" -- borrow from Python

Blobs
-----

Compound terms
==============

.. todo:: Note: We'll use the format foo[3,4] or foo[]

Functors
--------

.. todo:: old design at http://www.dyna.org/wiki/index.php?title=Atom
.. todo:: What space are these in?  How do they relate to strings and
	  dynabases?  See https://github.com/nwf/dyna/issues/49, and
	  consider RDF names.
.. todo:: local versus global functors

Single quotes
^^^^^^^^^^^^^
.. todo:: Escape codes as for strings except we need \' 
.. todo:: What space are these in?  How do they relate to strings and dynabases?  See https://github.com/nwf/dyna/issues/49]


Positional arguments
--------------------

Operator syntax
^^^^^^^^^^^^^^^

Keyword arguments
-----------------
.. todo:: see Python defaults, keywords, varargs at http://docs.python.org/3/tutorial/controlflow.html#more-on-defining-functions

List syntax
-----------

.. todo:: old documentation at http://www.dyna.org/wiki/index.php?title=Syntactic_sugar_for_terms
.. todo:: extracting functors and args; whether extracting a functor
	  from a dynabase or frozen term is null or an error

Reserved functors
-----------------

``$`` convention
^^^^^^^^^^^^^^^^

``$error``
^^^^^^^^^^

.. todo:: how to test for errors (catch errors) - maybe a forward
	  reference.  Trying to unify two errors -- including during
	  query or update propagation -- probably needs to give an
	  error, since otherwise errors might silently vanish when
	  they pass through equality, leading to unexpected results!
.. todo:: some discussion of $error in boolean section of
	  inference2.txt.
.. todo:: dealing with errors as arguments, e.g., f(X/Y) = ...
	  may yield f($error(...)).  Presumably this means that
	  f(X) yields some f($error(...)) matches: are their
	  values also errors?
.. todo:: We need to worry about extending the type system with error
	  values.  The definition `rational(:int P / :int Q).`
	  attempts to describe all ratios of integers as rational
	  (although we may not be able to plan this query unless we
	  have an enumerator for integers).  Since `Q` can be `0`,
	  this rule also defines `rational($error)`, where the `$error`
	  object is statically typed as a float because that's the
	  output type of division.  As a result, we should discover an
	  error when we query `rational(:float F)` but not when we
	  query `rational(:person P)` which is an unrelated use of
	  `rational` defined by other rules.

Errors are handled declaratively.  Thus, 
* errors have provenance;
* the errors that we get don't depend on the execution strategy (in
  the single-fixpoint world);
* if `f(1)` has an error value than `f(X)` will also return an error value.

There is at least one special `$error` value.  We may need many such
values that are particular error tokens (like gensyms) and carry
messages around and may have specific types.

Where errors originate:

* We define `assert(:bool B)` be `true` if `B` is `true` and
  `$error` otherwise.  (Perhaps including if the argument is `null`, in
  which case the argument actually has to be unevaluated as with `?B`.
  If not, the user can still write `assert(?B)` to have this meaning.)

* Built-ins may have error values.  For example, `$error` is the value
  of `1/0`.

* User programs may define some items to have an `$error` value.
  E.g., perhaps `prime` takes integer arguments but we say that
  `prime(N) = $error for N < 2`.  

* Items can be defined extensionally to have `$error` values.

* If a type has a side condition that must be checked at runtime, then
  failing that side condition at runtime generates an error.  For
  example, we might declare a range type of integers that are `>= 2`
  and we use that as the type of the argument to `prime`.  If the
  compiler cannot prove at compile time that the argument to `prime`
  satisfies the `>= 2` test, then it inserts a runtime assertion,
  replacing `prime(N)` with `assert(N >= 2), prime(N)`.

* Integrity constraints. 

How errors are consumed:

* By default, any item whose name is an `$error` or contains an
  `$error` has value `$error`.  In other words, if no value can be
  proved for such an item, its value is taken to be `$error` rather
  than null.  If a value of `$null` can be proved for such an item,
  then evaluation of the item does return null.  (Maybe better: if the
  item unifies with the head of any rule or can be set extensionally,
  then we skip the default behavior and just go with the rule(s) that
  define the value, possibly null.  Note that this means that we can't
  just clear out dead code -- rules that always fail -- because they
  might ensure that error terms have null values.)

* However, this is overridden by some built-ins.  For example, `true |
  $error` evalues to `true` and `0 * $error` evaluates to `0`.

* Similarly, a user program can trap errors.  For example,
  `times(0,$error) := 0`.  Also, `f(:int X) = 0` presumably means that
  `f($error)` yields 0.

So, is it an error to ask for `5 is 10/X`?  How about `5 is 10/0`?
(Note that `10 is X*5` and `10 is 0*5` are not errors; the latter just
fails.)

nwf's opinion is that these just fail (return null) without an error.
`10/0` returns a special `$error` value that doesn't unify with `5`.
Handling `$error` like an ordinary value will certainly make it easier
to catch exceptions by writing rules that unify with `$error`.  A
downside of the approach is that while `prime(0)` is an error, `true
is prime(0)` is simply null.

Another approach says that even attempting to unify a non-error with
`$error` gives an error.  In this case, it's an error to even inspect
the value of `prime(0)`.  And the query `f(5)` would match against a
rule head `f(10/0)`, giving an error.  Basically, deriving any value
for `f($error)` would add an `$error` aggregand to `contrib(f(X))` ...
This version may not work well because ordinary queries would crash
when encountering rules that define values for error terms, unless
those are somehow handled separately or unification is asymmetric.
Unfolding may also have this problem.

Example: `count += f(X), 1 for prime(X)`.  

We also have to figure out the default value of error terms (see above).



``$null``
^^^^^^^^^

.. todo:: special value for `:=` (where it overrides) and also for `=`
	  (where it prevents override by a later value).  But may also
	  need to use it for error handling (see above).  This use might
	  cause us to think about an alternative scheme where `$null`
	  collapses to null upon query, but that probably interacts
	  badly with folding/unfolding.

.. todo:: what happens when `$null` is used as an argument?

Sets
====

.. todo:: See sets and rsets in frozen3.txt

Dynabases
=========

Full discussion in :ref:`dynabase`.

Gensyms
-------

Frozen terms
============

Full discussion in :ref:`frozen`.

*********************************
Patterns (i.e., non-ground terms)
*********************************

.. todo:: old documentation is at http://www.dyna.org/wiki/index.php?title=Pattern

Variables
=========

.. todo:: Old documentation at http://www.dyna.org/wiki/index.php?title=Variable

Variable names
--------------

Underscores
-----------

Non-ground terms
================

Types
=====

Type declarations
-----------------

.. todo:: old design at http://www.dyna.org/wiki/index.php?title=Term#Union_types

Typed variables
---------------

Co-inductive types
------------------

.. todo:: how do we deal with types for varargs and keyword args?

Possible future extensions
--------------------------
Guarded types?  Nonlinear types?  Parametric types?

Dynamic (non-const) types, and relation to intern table implementations.

Type coercion
=============


Unification
===========

.. _frozen:  

Frozen terms
============

.. todo:: see frozen3.txt.  
    
.. _dynabase:  

*********
Dynabases
*********

Overview
========

Items
=====

.. todo:: old documentation at http://www.dyna.org/wiki/index.php?title=Item

Null items
----------

Syntax for items
================

.. todo:: possibly mention :ref:`dispos`

Brackets vs. parentheses
------------------------

Quoting items with ``&``
------------------------

Evaluating terms with ``*``
---------------------------

Assertions
==========

Queries
=======

Simple queries
--------------

.. todo:: what does a query for simply ``X`` get?  How about ``d.X``
	  or ``d->X`` where X is a dynabase?  Does it range over
	  primitives?  Expressions?


Complex queries
---------------
.. todo:: joins with commas, and "for" clauses

.. todo:: when is f(X),g(X) considered to be incompatible with the
	  types of f and g?  

Expressions
-----------
.. todo:: Call these "nested queries"?  Auto-evaluation.
	  Old discussion at
	  http://www.dyna.org/wiki/index.php?title=Expression and at
	  http://www.dyna.org/wiki/index.php?title=Syntactic_sugar_for_inference_rules .

.. todo:: when is f(g(X)) considered to be incompatible with the type
	  of f and the evaluation type of g?

.. todo:: do primitives evaluate to themselves, etc.

.. todo:: can you quote the * operator?  (What is the order of
	  desugaring?)

.. todo:: using prefix ^ to evaluate variables in the parent lexical scope.
	  See frozen3.txt.

Aggregating queries
-------------------

.. todo:: i.e., prefix aggregators.  original discussion at http://www.dyna.org/wiki/index.php?title=Syntactic_sugar_for_inference_rules
	  Also some discussion of prefix aggregators in thread "Quick
	  question about syntax", 1/10/14 .

.. todo:: use ^ to lift a variable out of the lexical scope of the
	  aggregator even if it's not mentioned outside.  For example,
	  ``query (+= foo(I,^J))`` will return a separate answer for
	  each J, e.g.
	    (+= foo(I,2)) = 5.
	    (+= foo(I,3)) = 7.
          And ``query (*= (+= foo(I,^J)))`` will return 35 in this
	  case.

.. todo:: getting a prefix aggregator to capture extra variables.
          See discussion in frozen3.txt, and at randomness below.
          

Accessors
---------
.. todo:: Dot and arrow expressions to access elements of a dynabase

Query modes
===========

Some discussion of current approach is in :doc:`/manual/pragmas`.

.. todo::      
      Language for specifying modes as generalization of types.
      Output format: Finite, sorted, consolidated, ground, etc.
      Det/semidet/etc.  Overall, or per-variable?  What if it binds a
      sequence of variables?

.. todo:: Discussion of query modes (to be supported in future) that 
	  return programs.  For example, between(0,X,1) might return
	  `{ between(0,X,4) := false.  between(0,0,1) := true.
	  between(0,1,1) := true. }`

.. todo:: error results

.. todo:: user might want to specify modes that are promised (speeds
	  up planner, like type declaration) and modes that are
	  withheld (says that planner shouldn't use these modes since
	  they're expensive or bloat the code or since we don't
	  promise that they'll be possible in a future version of the
	  program).  Can combine these by specifying modes
	  exhaustively.

.. todo:: When a dynabase is extended, it might support fewer modes
	  because the new rules can't be planned.  This means that the
	  extended dynabase has a more restricted type than the
	  original (i.e., a supertype) and can't be assigned to any
	  variable that requires the original type to successfully
	  do mode planning.

.. todo:: Dynamic query modes?  Maybe we can only query EM's
	  posteriors and parameters at timemax, defined by a max=.  So
	  an external query for a different time might be allowed by
	  the static API, but would give its error dynamically.  

	  Our internal query modes might also support only this query,
	  using the peeking value of timemax.  This allows us to save
	  on memory as we run, under an appropriate plan that proceeds
	  tier-by-tier and updates timemax only between tiers.  The
	  planner has to be very careful to keep track of the current
	  value of timemax that will enable the plan to succeed, and
	  careful when it updates timemax lest that prevent some
	  updates currently on the agenda from running.

Lambdas
=======

.. todo:: with keyword args.  should be sufficient for teaching NLP.

Terms as dynabases
==================
.. todo:: querying and destructuring terms

Updates
=======
.. todo:: Ownership [forward reference]

Update modes
============
.. todo:: must discuss aggregators as well as groundedness and form of
	  body

.. todo:: 

Stability
=========
.. todo:: guarantees on queries (what can change as a result of updates; is a := update a definite override?)

.. todo:: The following things might be unstable: ?=, builtins that
	  have free-choice like ?=, latching results, randomness,
	  gensym and dynabase identity.  We should always guarantee
	  that they are stable within a query.  When do we want to
	  guarantee that they are stable across multiple queries of
	  the same snapshot, or queries to (snapshots of) dynabases
	  that are related by "irrelevant" update or extension?  (and
	  how do we define "irrelevant"?)  They might also change
	  across multiple runs, e.g., if the same dynabase is loaded
	  twice.

.. todo:: Regarding latching, we might want to make a weak guarantee:
	  that we find a minimal fixpoint in the sense that null is
	  less than any other value.  This means that if we can find a
	  way to null out a bunch of values in the current fixpoint
	  and adjust other values (without converting anything from
	  null to non-null) so that everything becomes consistent,
	  then we must do so.  Can we make this guarantee for all
	  programs, or only for programs with stratified negation?

.. todo:: Something stronger than stable would be "replicable," which
	  forces all loads of the Dyna program to get the same
	  fixpoint (regardless of the order of queries, the machine
	  it's run on, etc.).  This means that we need a deterministic
	  way to add timestamps to items so as to break cycles.  (And
	  also a deterministic way to set random number seeds and
	  decide how to handle ?=.  These have to be voodoo-free.)
	  This might be done by defaults, overridable by pragmas.  The
	  timestamp on x^{(T)} means that it's the value when x is
	  updated for the Tth time during forward chaining.  This
	  timestamping can create an infinitely deep circuit -- but we
	  can answer a query of x with respect to any x^{(t)} that has
	  converged, meaning that it equals x^{(t+i)} for any i > 0.
	  This is guaranteed by failing to prove non-convergence:
	  x^{(t)} is active (non-converged) if x^{(t+1)} is different
	  from x^{(t)} or has an active antecedent.  For example,
	  convert `x=y. y=x.` into `x(T+1)=y(T). y(T)=x(T),`.  Note
	  that `x(0)` and `y(0)` are explicitly null (or some other
	  deterministic guess).  We need to ensure that the
	  timestamping conversion ensures that the timestamps increase
	  by 1 at each step (may be unfortunate if there are many
	  updates that don't do anything because the rule contributes
	  a null aggregand), or at least increase.  In other words,
	  x(t) can't have x(t') as transecedent for any t' > t, and
	  perhaps it should have at least one x(t-1) transecedent.

.. todo:: We need a declaration language and API that determines when
	  things are released from being stable, etc.  For example,
	  perhaps a fresh load of a dynabase is allowed to get all
	  new values.  Can stable values depend on unstable values,
	  especially in another dynabase, and we treat changes to
	  those values as akin to updates?  Or 
	  
	  

Dynabase types
==============
.. todo:: distinguish type from mode=(contents,queries,updates).  See message from Tim on
	  4/23/14, Subject: "white board".  Also see message from me
	  on 7/2/14, Subject: "Meeting tomorrow"

.. todo:: integrity constraints.  generally hold at convergence, not
	  necessarily transiently.  May include functional dependency
	  or cardinality constraints.  (Note: Hard to take advantage
	  of integrity constraints in storage layer because they might
	  fail transiently.  Sometimes we might be able to prove that
	  they will hold transiently under all updates.  But usually,
	  we're better off just optimizing storage for common cases
	  while still allowing rare cases via overflow pointer etc.)

Extensions
==========

Const declaration
-----------------

Snapshots
=========

**********************************
Inspecting and modifying dynabases
**********************************

Abstract API
============

.. todo:: this is really described above.
.. todo:: Discuss ad hoc, continuous, peeking queries.

Command line interface
======================

Graphical interface
===================

.. todo:: old Dynasty description and links at
	  http://www.dyna.org/wiki/index.php?title=Dynasty .
	  Over 216 tickets on the Dynasty queue at
	  http://bugs.dyna.org, with lots of design discussion.

Programming interface
=====================

*************
Dyna programs
*************

Overview
========

.. todo:: Old description at http://www.dyna.org/wiki/index.php?title=Program

File format
===========
.. todo:: UTF-8, BOM
.. todo:: #! and possibly multiline #!

Rules
=====

.. todo:: Old documentation at http://www.dyna.org/wiki/index.php?title=Inference_rule

Definition
----------

Aggregation
-----------

Guards
------

.. todo:: refer back to guards

Fixpoint semantics
------------------

.. todo:: perhaps also discuss execution policies

Errors
^^^^^^

.. todo:: See discussion of current error policy in :doc:`/tutorial/errors`.

Cycles
^^^^^^

Stability
^^^^^^^^^

to nwf:
   I think I really want the semantics of dynabase extension
   to be the same as the semantics of updates.  This means that
   when you extend a dynabase, you don't disturb randomness or
   cycles that are not transequents of your extension.  Whether
   dynabases become new in an update is not answerable from
   within Dyna, I think, because the old dynabase can no longer
   be accessed; but it should be answerable from outside, e.g.,
   d.e may be different before and after we update d.e.x += 1.
   This may be up to the implementation.  (Certainly snapshots
   of those two are different.)  But maybe two snapshots both
   before the update should be the same.

      (All queries are against snapshots, so how do we actually get a
      live version of e?  Maybe we can't.  Or maybe queries are not
      totally against snapshots after all -- the snapshot is taken only
      to get the path to e but the thing that that path points to is live
      by default, so the recursive snapshotting doesn't go all the way
      down.  Anyway, if we do get two snapshots of e before and after,
      then they must be different in the sense that they get different
      results when we query them.)


See discussion of current implementation in :doc:`/tutorial/errors`.

Gensyms
-------

.. todo:: how gensyms capture variables.  This is actually the case for the "new" operator in general.

.. todo:: Variables whose names start with _ (including _ itself) are not captured.

.. todo:: Note that ^* is well-defined and affects capturing.

.. todo:: Interaction of capturing with prefix aggregators: Consider the expression `(+= f(A,B)*weight(*) for A in set) + g(B) + h(C)`.  In the subexpression `(+= f(A,B)*weight(*))`, both A and B are captured by the gensym, just as if we had lifted out the rule `temp(B) += f(A,B)*weight(*)`.    

Head destructuring
------------------

Dynabase literals
=================

Syntax
------

.. todo:: using ^ to construct and evaluate terms in the parent
	  lexical scope

Ownership
---------

Semantics
---------

Declarations
============

Some documentation of currently implemented declarations is in :doc:`/manual/pragmas`.

.. todo:: old design: see http://www.dyna.org/wiki/index.php?title=List_of_declarations

Type declarations
-----------------
.. todo:: [backward reference] - maybe uses const defs

.. _dispos:

Evaluation declarations
-----------------------

There is currently some documentation in :doc:`syntax`.


Default arguments
-----------------
.. todo:: in constructors


Visibility declarations
-----------------------
.. todo:: readable, writeable

Const
-----

Import
------

Syntax declarations
-------------------
.. todo:: forward ref
.. todo:: Which things affect only subsequent lines?

Declaring new aggregators
-------------------------
.. todo:: Hard case: Should be possible to construct a new aggregator based on a
	  binary operator that is defined at runtime in a semiring dynabase.

Declaration inference [elaboration]
===================================

.. todo:: and compile-time errors.  See
	  http://www.dyna.org/wiki/index.php?title=Declaration_inference

.. todo:: Declared types of functors are exact, i.e., they are both
	  upper and lower bounds.  Inferred types of functors are
	  obtained by forward chaining abstract interpretation to get
	  lower bounds (whereas type checking does backward chaining
	  with a cosmetic plan to get upper bounds).  We may also have
	  to do inference to understand the type declarations, e.g.,
	  declaring the type `f[g[X]]` may use an inferred type for
	  the `g[X]` argument, and this type may be determined from
	  rules.

	  When doing type inference, we inflate literals to their
	  "natural types" (replacing them with typed variables).  This
	  happens by replacing a literal such as `2` with an item
	  `someint` of type `:int`.  (Or perhaps by a variable `:int
	  _`, but then the forward chaining might mistakenly think
	  that infinite aggregation is going on and be influenced by
	  that; by using `someint` we give it access to the
	  information that the literal is a single deterministic
	  value.)  

	  During inference, we may do lookahead on the 

If we have a rule `f(1).` then we act as if
	  the rule were `f(:int _).`, and we should get the same
	  behavior if it were `f(X) :- X=1.`, in other words, `f(X) :=
	  X = :int Y.`.  It's possible that types might get restricted
	  in order to pass type checking, e.g., `f(X) :- X=1, g(X).`
	  where `g(X)` restricts the type of `X` further because it
	  only takes `#0|#1`.

	  
   Type checking: when we encounter a subgoal F is f(X,Y,Z), and only
   F and Z are first-use variables, then we determine the possible
   values of (Z,F) such that f(X,Y,Z) is well-formed for *any* value
   of (X,Y) allowed by the current type.  This becomes the type of
   (Z,F), and we fail type-checking if it is empty.  Actually we must
   get a joint type of (X,Y,Z,F).  An explicit type restrictor
   :ty V restricts the type of V and again we fail type-checking
   if the result is empty.  That is equivalent to saying that 
   this copy of V by identity_ty(V) declared as
       :ty identity_ty(:term V)
   which takes any term and returns a copy of it if it's in the type,
   otherwise returns null.  That means that we really have 
       V' is identity_ty(V)
   as a subgoal and we use V' instead of V.  FURTHERMORE, we 
   use V' instead of V everywhere later in the rule.

Type inference [elaboration] on variables
-----------------------------------------

.. todo:: If types have been declared (or inferred) on functors, then
	  this imposes implicit restrictions on variables that are
	  used as arguments to those functors.  Specifically, in each
	  subgoal of a rule, we consider variables that are being used
	  for the first time anywhere in that subgoal (including
	  inside nested terms and nested evaluations!).  We jointly
	  infer types for all these variables given the declarations
	  of functors at their parents and the types of other
	  variables.  For example, consider
	  f(g(p(X,r(Y)),h(q(s(X),Y,t(A))))) where X,Y are first used
	  here.  We already know the type of A, therefore the return
	  type T of t(A) (which must be not only constructable but
	  also evaluable for all A in its type, or we get an error).
	  We now jointly infer maximal types for X, Y, R, S.  These
	  are the intended types of 

	  We don't yet know the return types R of r(X) or S of s(Y),
	  but we have upper bounds on them.

Type inference [elaboration] on functors
----------------------------------------

.. todo:: if the user defines f(0) and f(1), should we guess
	  that f only takes integers, so that it's a type error
	  to query f("hello") from the repl or as a subgoal, or 
	  to update the dynabase with such an aggregand?
	  What if the user defines f(&g(3))?
	  Are we to generalize from examples to the minimal 
	  basic or natural types that cover those examples?

	  Answer:
	  Yes, we should construct a lattice of "natural" types:
	  * built-ins (int, rational, string, etc.)
	  * natural subtypes of built-ins (e.g., nonnegative int)
	  * functor-arity types like f/3 with other natural
	    types as their arguments
	  * term
	  * all user-declared types in this dynabase
          * possibly some inferred types such as recursive types
	  * all finite unions and intersections of the types above, 
	    so that we have a lattice

	  Thus, we have a notion of the least "natural" upper bound on
	  a non-ground term.  A constant like `f(3,3)` can be
	  generalized to `f(:int X,:int X)` if a type `f(X,X)` is
	  declared, otherwise to `f(:int X, :int Y)` by recursing on
	  the arguments.

	  The user may declare types for some terms and items.  We
	  desugar each rule using these declarations.  If the head 
	  has variables, then we restrict them according to any
	  declarations of the head term, a

	  according to the way that they're declared), 
	  Type inference proceeds by forward chaining from body to
	  head, in order to inflate lower bounds on the item type (the
	  `is` terms).  We inflate the head to the least "natural"
	  upper bound, i.e., if we have a rule `f(1) = false` then we
	  always `:bool is f(:int)` in the item type.  This allows
	  the user to query `f(3)` and use the result in a boolean
	  expression, even though the result will definitely be null
	  with the current version of the program.

	  (However, for items that only appear in the body and are not
	  declared as updatable, we have to guess their update types
	  based on usage.  We want to guess the maximal type that will
	  type-check.  For example, if `g(0)+1` appears as a subgoal,
	  then we know that `g` must be able to take int (lower bound)
	  and must return at most int (upper bound), 

	  We later do type checking based on left-to-right ordering.
	  It must be possible to construct each term based on the type
	  of the binding of previously used variables
	  


          If our type inference proves that a particular collection of
	  terms can be items, then we relax the inferred type to the
	  least "natural" upper bound (i.e., the lub in this lattice).
	  This lub is never `term` unless the inferred type was term.
	  This relaxation might happen during type inference.

	  Type inference has to get a type for items, i.e., (name,value) pairs.
	  The term type (within this dynabase) should be the set of
	  types of subterms of these item pairs ... although it's not
	  clear that foreign terms themselves count, only the terms
	  that they are part of.

	  Type checking is different from this.

	  Infer recursive types: with the rules `f(s(X)) = f(X).` and
	  `f(z) = 3.`, we probably say that the type tau of the argument
	  to f must contain both z and s(tau).  This constructs an
	  automaton where the tau state receives a z hyperarc with no
	  inputs and an s hyperarc from itself.

	  If the user writes `f(0)=3. f(1)="hello".` then is it a
	  likely error?  Should we give a warning if we are forced up
	  to an undeclared union type in the value or the argument?

.. todo:: can we infer parametric types?

Aggregator declaration inference [elaboration]
----------------------------------------------

.. todo:: similar to above, if the user says that += is the
	  aggregator for f(0), do we conclude that it is
	  the aggregator for f?  for all f(int)?  if f(0) and
	  f(1) have different aggregators, is that an error/warning
	  unless otherwise declared?  In general it may be ok to 
	  have a separate consistent aggregator for each item,
	  but if the repl can create new aggregators for things
	  that feed into a rule, we might have to recompile the
	  way we handle updates for those things ...

Scripting commands
==================

.. todo:: Maybe this should be in another section.  Somewhere, we need to describe the repl and notebook interfaces.  (These are very closely related: a .dyna file is like only the input to a notebook, and a session with the repl is logged to a .dyna file.  Editing the .dyna file is like changing the notebook in place (it's just that the old input and output can't be seen).)

Include
=======
.. todo:: what is interaction with syntax table?


Foreign function interface
==========================

***************
Concrete syntax
***************

Overview
========

Standard syntactic sugar
========================

.. todo:: semicolons

.. todo:: prefix aggregators and ^

.. todo:: type restrictors on variables and on evaluated and quoted subexpressions 

.. todo:: some ideas at
	  http://www.dyna.org/wiki/index.php?title=List_of_keywords

.. todo:: "in" (overloaded for lists, ranges, sets, frozen terms,
	  possibly maps or general dynabases)

Default syntax table
====================

Changing the syntax table
=========================

.. todo:: old documentation of declaring new operators (similar to
	  Prolog) is at
	  http://www.dyna.org/wiki/index.php?title=Syntactic_sugar_for_terms.
	  A possible new design is in :doc:`/manual/pragmas`.

.. todo:: nwf started working with Jay's parser: http://bugs.dyna.org/rt/Ticket/Display.html?id=588

Printing
========

Readable printing
-----------------

Prettyprinting
--------------

****************
Standard library
****************

There is currently some documentation in :doc:`/manual/builtins`.

.. todo:: in general, builtins presumably act as if they were aggregated by `=`,
	  so that they can't be overridden.  We might allow the user
	  to declare otherwise but it's dangerous.

Generic operators and aggregators
=================================

.. todo:: :=, =, ?=, comma/for
      Note that `?=` when distributed over multiple rules might
      implicitly do `;; $null` for each rule, so that if one branch
      fails then we are not required to fall back to other branches
      That generally isn't the case within a rule, e.g.,
      `arbitrary_member(set) ?= X for X in set` should not be allowed
      to return null by choosing some `X` not in the set (which
      makes the body null).

Boolean operators and aggregators
=================================

.. todo:: note tricks like forcing null to false by using `||
	  false`.  Or in the 2-valued case where false is not
	  possible, `?` can be used.  See inference2.txt.

Numeric operators and aggregators
=================================

.. todo:: include discussion of with_key and with_val.  In general
	  we may want some kind of multiple-value-return stuff (related to
	  coercion), which would also be useful for the Lisp examples
	  and for doing a version of member that allows duplicates by
	  binding additional hidden variables that refer to the entry
	  (that is rather like with_key).

.. todo:: also k-best derivations: http://bugs.dyna.org/rt/Ticket/Display.html?id=257

Randomness
==========

.. todo:: Import rand, and create a new seeded source of randomness
          via r = rand(seed) where seed may be either a constant term
          (for replicability) or * (which suggests new randomness on
          each run, although I'm not sure whether that's guaranteed).
	  Note that seed can be explcitily overridden in an extension if desired,
	  and we could use seed1 and seed2 for different distributions,
	  so that the extension can override one and not the other.
          Now for each term T, `g = r.gaussian(T)` is a different
          Gaussian-distributed random variable.  Usually T is * so
          that we have a new random variable on each run; maybe we can
          have * as a default argument?  Perhaps better, we should
          just do `g = new r.gaussian` to get a new random variable.  If
          we want to ensure replicability, then we must arrange for
          the `new` operator to actually put the captured context into
          the new dynabase (as private fields that can only be
          accessed by the new dynabase, and do not need to be stored
          if the new dynbase doesn't need them, as with `*`); the
          rules that define a Gaussian can then combine these fields
          with the seed to get a deterministic key.  Now, we can 
	  take multiple samples from `g` via `g.sample(I)` for 
	  indices `I`.  We can also combine `g` with other random
	  variables using overloaded operators, and observe the results.
	  The results are now 

.. todo:: The syntax `rand(*).gaussian(*).sample(*)` is rather clunky
          for just getting a Gaussian variate in the usual way.
	  Even if we start with `r = rand(*)` at the top of the file,
	  and then `strength(:person X) = r.gaussian(X)` to say that
	  strengths are Gaussian RVs, we still have to write
	  `strength("Atlas").sample(*)`.  So let's have syntactic
	  sugar for sampling from a distribution: `a ~ strength("Atlas")`
	  or more directly `a ~ r.gaussian(*)`.  Presumably,
	  the aggregation operator puts the `.sample(*)` part into the
	  injector.  Note that `~` can be used as a prefix operator if 
	  we don't want to name our samples, e.g., 
	  `mean= (~ strength(X)) for myfriend(X).`
	  For multiple samples from the same variable, we might
	  want explicitly `mean= strength("Atlas").sample(I) for 1 <= I <= 100`.
	  Maybe there should be a nicer way to generate a vector of samples.


String operators and aggregators
================================

Array operators and aggregators
===============================

Set operators and aggregators
=============================

Graph operators and aggregators
===============================

Other standard encodings
========================
.. todo:: see ~/jot/dyna 12/14/12 et seq.
   
***************************
Analyzing program execution
***************************

$rule
=====

$gradient
=========

.. todo:: Rationale at
	  http://www.dyna.org/wiki/index.php?title=Training_weights
.. todo:: Old design at http://www.dyna.org/wiki/index.php?title=DynaMITE

Voodoo items
============

Reflection
==========

.. todo:: reflection on types, modes, cost estimates, cardinality estimates, plans, etc.

*****************************
Controlling program execution
*****************************

Storage classes
===============

Priorities
==========

Query costs and plans
=====================

Features for learning
=====================
   
*****************
Foreign dynabases
*****************

Files
=====

Processes
=========

Sockets
=======

Servers
=======

**********
Appendices
**********

.. toctree::

   errors
   glossary




