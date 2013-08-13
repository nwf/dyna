.. -*- compile-command: "make -C .. html" -*-
.. Specification index
   This file is enumerated in the toctree directive of index.rst

.. todo::
   Fill in to-do items from various sources, then break this into
   multiple files.  (I believe that in the HTML rendering, each file is a webpage.)

##################################
Specification of the Dyna Language
##################################

************
Introduction
************

What is Dyna?
=============

.. todo:: keep it brief

Intended users
==============

Key features
============

Relation to other work
======================

.. todo:: where to learn more

********************************
 How to read this specification
********************************

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
.. todo: deprecation, bugs, future work

Links to examples
=================

Links to issue tracker
======================

Glossary/Index
==============

***************************
Terms (i.e.,  ground terms)
***************************

Overview
========
      
Primitive terms
===============

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

List syntax
-----------

Reserved functors
-----------------

``$`` convention
^^^^^^^^^^^^^^^^

``$error``
^^^^^^^^^^

``$null``
^^^^^^^^^

Dynabases
=========

Full discussion in :ref:`frozen`.

Frozen terms
============

Full discussion in :ref:`dynabase`.


*********************************
Patterns (i.e., non-ground terms)
*********************************

Variables
=========

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

Typed variables
---------------

Co-inductive types
------------------

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
    
.. _dynabase:  

*********
Dynabases
*********

Overview
========


Items
=====

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

Queries
=======

Simple queries
--------------

Complex queries
---------------
.. todo:: joins with commas

Expressions
-----------
.. todo:: (auto-evaluation) ("nested queries")

Aggregating queries
-------------------

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

Dynabase types
==============
.. todo:: i.e., advertised public interface

Extensions
==========

Const declaration
-----------------

Snapshots
=========

************************************
 Inspecting and modifying dynabases
************************************

Abstract API
============

.. todo:: this is really described above.
.. todo:: Discuss ad hoc, continuous, peeking queries.

Command line interface
======================

Graphical interface
===================

Programming interface
=====================

***************
 Dyna programs
***************


Programs
========

File format
===========
.. todo:: UTF-8, BOM
.. todo:: #! and possibly multiline #!

Rules
=====

Definition
----------

Aggregation
-----------

Semantics
---------

Cycles
^^^^^^

Errors
^^^^^^

See discussion of current implementation in :doc:`/tutorial/errors`.

Head destructuring
------------------

Dynabase literals
=================

Syntax
------

Ownership
---------

Semantics
---------

Declarations
============

Some documentation of currently implemented declarations is in :doc:`/manual/pragmas`.


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


Scripting commands
==================

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

Default syntax table
====================

Changing the syntax table
=========================

Printing
========

Readable printing
-----------------

Prettyprinting
--------------

******************
 Standard library
******************

There is currently some documentation in :doc:`/manual/builtins`.

Generic operators and aggregators
=================================

.. todo:: :=, =, ?=, comma/for

Boolean operators and aggregators
=================================

Numeric operators and aggregators
=================================

Randomness
==========

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
   
******************************
 Inspecting program execution
******************************

$rule
=====

Voodoo items
============

Reflection on types, modes, cost estimates, cardinality estimates, plans, etc.
==============================================================================

*******************************
 Controlling program execution
*******************************

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

   glossary




