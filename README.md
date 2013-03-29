An overview of the source tree
------------------------------

* examples/            -- Dyna source programs
    *  expected/       -- Expected output for self-tests.  Named by program and backend, both.
* external/
    * damsl-k3         -- The DAMSL K3 tree, tracked as a git submodule.
* src/Dyna/
    * Analysis         -- The heart of the compiler
        * Mode         -- A re-implementation of the Mercury mode system
    * Backend          -- Compilation to target languages
        * K3           -- An AST and printer for K3, done in finally-tagless style.
        * Python       -- A Python code generator
    * Main             -- Dyna compiler drivers (main and test) and definitions used throughout the pipeline
    * ParserHS         -- the Haskell front-end parser and selftests
    * Term             -- Different representations of terms and utilities
    * XXX              -- code that should probably go upstream or be made freestanding.

Building
--------

First, ensure that you have the Haskell platform 2012.2 or later installed,
either through your favorite package manager or by installing it
stand-alone.  You should probably run

    cabal update

before proceeding, just to make sure that your package database is
up-to-date.  Some of our transitive dependencies assume that you have
`alex` and `happy` available -- either fetch those from your package manager
or add `~/.cabal/bin` to your `PATH` and run

    cabal install alex happy

Then fetch, build, and install any dependencies (for the
moment, we seem to be doing OK with vanilla upstreams!)

    make deps

Build Dyna:

    make build

(If that doesn't work, you might try `make ghcbuild` which ignores the Cabal
infrastructure.  If you get an error about the `ghc-prim` package, it means
that your compiler is older than what I've tested on; you may be able to
make things work by dropping the lower bound on the package in dyna.cabal.)

You'll want to have the following programs installed, too:

    * IPython
    * Pygments (for pretty code output in our debugging tools)

Run the test harness:

    make tests

At this point, the code is still rather "in the works" so you probably want
to...

* load some module in GHCi; for example:

        ghci -isrc Dyna.ParserHS.Parser

* Run the python backend interactively (leave off the "-i" for bulk
operation):

        ./dyna -i examples/papa2.dyna

* Produce visualizations of some of the internal stages of our compiler

        ./debug examples/papa2.dyna

Disclaimer
----------

This may someday be useful.  For the moment, it doesn't do much except keep
us busy.  If you're trying to make it do something and it breaks, you get to
keep all the pieces; see sections 15 - 17 of the AGPLv3 (available in
LICENSE).
