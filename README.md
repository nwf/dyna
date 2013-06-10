An overview of the source tree
------------------------------

* docs/sphinx/         -- Documentation in reST format for sphinx
* examples/            -- Dyna source programs
    *  expected/       -- Expected output for self-tests.
* misc/                -- Pretty much what it says on the tin
* src/Dyna/
    * Analysis         -- The heart of the compiler
        * Mode         -- A re-implementation of the Mercury mode system
    * Backend          -- Compilation to target languages
        * Python       -- A Python code generator
    * Main             -- Dyna compiler drivers (main and test) and definitions used throughout the pipeline
    * ParserHS         -- the Haskell front-end parser and selftests
    * Term             -- Different representations of terms and utilities
    * XXX              -- code that should probably go upstream or be made freestanding.

Building
--------

First, ensure that you have the Haskell platform 2012.2 or later installed,
either through your favorite package manager or by installing it stand-alone.
You'll want to have the following programs installed:

    * Python 2.7 or compatible
    * graphviz

The python modules required

    $ easy_install numpy ipython pygments path.py

You should probably run

    cabal update

before proceeding, just to make sure that your package database is
up-to-date.  Running

    make

will then build dependencies and the Dyna executable.  Run the test harness,
just to make sure things built and are working OK:

    make tests

And read up on the documentation:

    make sphinxdoc

At this point, the code is still rather "in the works" so you probably want
to...

* Run the python backend interactively (leave off the "-i" for bulk operation):

        ./dyna -i examples/papa2.dyna

* Produce visualizations of some of the internal stages of our compiler

        ./debug examples/papa2.dyna

* load some module in GHCi; for example:

        ghci -isrc Dyna.ParserHS.Parser

Disclaimer
----------

This may someday be useful.  For the moment, it doesn't do much except keep
us busy.  If you're trying to make it do something and it breaks, you get to
keep all the pieces; see sections 15 - 17 of the AGPLv3 (available in
LICENSE).
