.. Spec glossary
   This file is enumerated in the toctree of spec/index.rst

Specification Glossary
######################

.. glossary::

   functor

     The constructor of a term, such as ``path`` in ``path(1,2)``.

   null

     The value of items that have no rules contributing aggregands.
     Null *annihilates* expressions (*e.g.* :math:`\mbox{null} + 2`
     is :math:`\mbox{null}`) and is the *unit* of aggregations
     (*e.g.* :math:`\sum\{\mbox{null}, 1, \mbox{null}, 2\}` is
     just :math:`\sum\{1,2\}`).
