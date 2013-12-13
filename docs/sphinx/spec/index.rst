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

Strings
-------

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
	  error, or errors might silently vanish when they pass
	  through equality, leading to unexpected results!
.. todo:: dealing with errors as arguments, e.g., f(X/Y) = ...
	  may yield f($error(...)).  Presumably this means that
	  f(X) yields some f($error(...)) matches: are their
	  values also errors?
	  

``$null``
^^^^^^^^^

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
.. todo:: use ^ to lift a variable out of the lexical scope of the
	  aggregator even if it's not mentioned outside.  For example,
	  ``query (+= foo(I,^J))`` will return a separate answer for
	  each J, e.g.
	    (+= foo(I,2)) = 5.
	    (+= foo(I,3)) = 7.
          And ``query (*= (+= foo(I,^J)))`` will return 35 in this case.
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
      Det/semidet/etc.  Overall, or per-variable?  What if it binds a sequence of variables?

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

Lambdas
=======

Terms as dynabases
==================
.. todo:: querying and destructuring terms

Updates
=======
.. todo:: Ownership [forward reference]

Update modes
============
.. todo:: must discuss aggregators as well as groundedness and form of body

Stability
=========
.. todo:: guarantees on queries (what can change as a result of updates; is a := update a definite override?)

.. todo:: The following things might be unstable: ?=, builtins that have free-choice like ?=, latching results, randomness, gensym and dynabase identity.  We should always guarantee that they are stable within a query.  When do we want to guarantee that they are stable across multiple queries of the same snapshot, or queries to (snapshots of) dynabases that are related by "irrelevant" update or extension?  (and how do we define "irrelevant"?)  They might also change across multiple runs, e.g., if the same dynabase is loaded twice.

Dynabase types
==============
.. todo:: i.e., advertised public interface

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

Declaration inference
=====================

.. todo:: and compile-time errors.  See
	  http://www.dyna.org/wiki/index.php?title=Declaration_inference

Type inference on variables
---------------------------

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

.. todo:: 

Type inference on functors
--------------------------

.. todo:: if the user defines f(0) and f(1), should we guess
	  that f only takes integers, so that it's a type error
	  to query f("hello") from the repl or as a subgoal, or 
	  to update the dynabase with such an aggregand?
	  What if the user defines f(&g(3))?
	  Are we to generalize from examples to the minimal 
	  basic or natural types that cover those examples?

.. todo:: can we infer parametric types?

Aggregator inference
--------------------

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

Generic operators and aggregators
=================================

.. todo:: :=, =, ?=, comma/for

Boolean operators and aggregators
=================================

.. todo:: note tricks like forcing null to false by using ``||
	  false``.  Or in the 2-valued case where false is not
	  possible, ``?`` can be used.

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




