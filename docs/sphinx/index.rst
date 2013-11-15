.. -*- compile-command: "make html" -*-
.. Dyna documentation master file

.. todo:: use a theme.  At the moment I followed the suggestion in
	  sampledoc to copy stuff from the sphinx documentation, but
	  nowadays there is explicit support for themes (I like
	  "agogo" or "sphinxdoc"), as explained in the sphinx manual.

.. todo:: how to obtain / install Dyna.  Also, licensing (see
	  http://bugs.dyna.org/rt/Ticket/Display.html?id=52, also
	  http://bugs.dyna.org/rt/Ticket/Display.html?id=569).

Welcome to Dyna!
=================================

Dyna is an new declarative programming language developed at
`Johns Hopkins University <http://cs.jhu.edu>`_.

This site documents the new version being developed at http://github.com/nwf/dyna.
The new version has been used to teach but is not yet complete or
efficient; you may file issues at http://github.com/nwf/dyna/issues.
An older design with a fairly efficient compiler can be found at http://dyna.org.  

.. warning:: Please be advised that this documentation, the implementation,
   and indeed the language itself are rapidly changing.

.. warning:: Some programs may not terminate.  Control-C will
   interrupt the program's execution.

Contents:

.. toctree::
   :maxdepth: 1

   tutorial/index

   manual/index

   spec/index

   Bibliography <bib>

.. todo:: add a "cookbook" that shows how to accomplish various common tasks
.. todo:: not sure whether we want a top-level bibliography
.. todo:: We may want to generate a separate PDF file for each directory


Indices and tables
==================

* :doc:`spec/errors`
* :doc:`spec/glossary`
* :ref:`genindex`
* :ref:`search`

.. todo:: should we also try to add something like the automatic
	  Python module index (modindex), but for dynabases?
