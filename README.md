Dyna
----

Dyna is a small, high-level declarative programming language. Dyna programs
specify a collection of named data items. These item's can be queried, much like
a database. Dyna's power comes from the ability to define an item's value by
relating other items with an equation. These possibly recursive equations
specify the logical structure of a computation. Thus, Dyna provides a unified
way of specifying both data and algorithms. Dyna's algorithms are reactive---a
change to an item's value efficiently propagates throughout the dynabase so that
all equations are satisfied.


Quick Start
-----------

First, ensure that you have the Haskell platform 2012.2 or later installed
either through your favorite package manager (e.g., `apt-get install
haskell-platform`) or by installing it stand-alone from the haskell homepage.

This is enough to get you the compiler up and running. In order execute
programs, you'll need to set up the Python backend. For that you'll need `Python
2.7+` and the following python modules

    $ easy_install ipython

Optionally, installing the following will enable certain extension

    $ easy_install pygments matplotlib numpy
    $ apt-get install graphviz

Now you're ready to build.

    make

this will build dependencies and the Dyna compiler executable.  Run the test
harness, just to make sure things built and are working:

    make tests

And read up on the documentation:

    make sphinxdoc

Now you're ready to run a Dyna program. We've included a few for you to play
with in the `examples/` directory.

* Run the python backend interactively (leave off the "-i" for bulk operation):

        ./dyna -i examples/papa2.dyna


Disclaimer
----------

This may someday be useful.  For the moment, it doesn't do much except keep
us busy.  If you're trying to make it do something and it breaks, you get to
keep all the pieces; see sections 15 - 17 of the AGPLv3 (available in
LICENSE).
