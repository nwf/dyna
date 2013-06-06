.. Syntax

******
Syntax
******

This chapter defines Dyna's concrete syntax.

.. index::
   single: quotation and evaluation
   single: syntax; quotation and evaluation

.. _syntax-quote-eval:

#############
Quote vs Eval
#############

.. sidebar:: Relevant source files:
   
   * :dynasrc:`src/Dyna/Analysis/ANF.hs`
   * :dynasrc:`src/Dyna/Term/SurfaceSyntax.hs`

Dyna's syntax has both *nested terms* (also called *recursive terms*) and
*in-place evaluation*.  (Recall the discussion in
:ref:`pragma_disposition`.)  Managing the details in a way that is (ideally)
not too surprising to users requires some technical complexity.

Since this all takes place at parse time, the most upon which we can really
expect to key our decisions is the term's functor and arity.  Each
(functor,arity) pair may specify

* A :term:`self disposition`, which may be one of ``evaluate``, ``quote``,
  or ``inherit``.  No functors in Dyna (by default) use ``evaluate``; it is
  offered to facilitate the development of more Prolog-like syntaxes.

* For each argument, a :term:`argument disposition` which may be either
  ``evaluate`` or ``quote``.

Additionally, there are two explicit operators defined in the language:

.. index::
   single: operators; quote.
   single: operators; evaluate.

* A quotation operator, prefix unary ``&``.

* An evaluation operator, prefix unary ``*``.

When attempting to understand a term in a Dyna program, one must keep track
of:

* The argument disposition of the location where it occurs.  Functors
  specify this as per above; an aggregator will always place its head in a
  quoted context and its body in an evaluation context.

* The number of ``*`` operators seen between the functor's argument position
  and the functor of the inner term.

* Whether or not the sequence of quotation and evaluation operators ends
  with a quotation operator.  Note that only the right end matters; that is,
  ``*&*&`` has the same effect as ``**&``.

* The self disposition of the inner functor.

The interpretation is then the first matching row in this table:

======== ============== ======= ========= ==================================
Context  Eval Operators Quoted? Self             Effect
======== ============== ======= ========= ==================================
Any      0              Yes     Any       Quotation (explicit at site)
Any      0              No      Quote     Quotation (implicit from self)
Any      0              No      Evaluate  Evaluation (implicit from self)
Quote    0              No      Inherit   Quotation (implicit from context)
Evaluate 0              No      Inherit   Evaluation (implicit from context)
Any      :math:`n > 0`  Yes     Any       :math:`n`-chained evaluation
Any      :math:`n > 0`  No      Evaluate  :math:`(n+1)`-chained evaluation
Any      :math:`n > 0`  No      Any       :math:`n`-chained evaluation
======== ============== ======= ========= ==================================

Where, by ":math:`n`-chained evaluation", we mean one evaluation of the term
at hand, and then :math:`n-1`
:term:`indirect evaluations <indirect evaluation>` where the :term:`value`
is fed through the :term:`chart` to obtain the next value.  The last of
these is taken to be the value of the chain as a whole.  See
:ref:`spec-indirect-evaluation` for more details.
