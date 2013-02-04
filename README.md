An overview of the source tree
------------------------------

* examples/            -- Dyna source programs
    *  expected/       -- Expected output for self-tests.  Named by program and backend, both.
* external/
    * damsl-k3         -- The DAMSL K3 tree, tracked as a git submodule.
    * ekmett-parsers   -- ekmett's parsers combinator library, tracked as a git submodule.
    * ekmett-trifecta  -- ekmett's trifecta parser combinator library, tracked as a git submodule.
* src/Dyna/
    * Backend
        * K3           -- An AST and printer for K3, done in finally-tagless style.
        * Python       -- A Python code generator
    * Backend          -- Compilation to target languages
    * Main             -- Dyna compiler drivers (main and test) and definitions used throughout the pipeline
    * ParserHS         -- the Haskell front-end parser and selftests
    * Term             -- Different representations of terms and utilities
    * XXX              -- code that should probably go upstream; modules here are named by the upstream package.

Building
--------

First, ensure that you have GHC 7.6 or later.  (Though in a pinch, if you're
only interested in the frontend stuff and the Python backend, apparently as
early as 7.0 continues to be servicable.)

Build K3, if that's your thing, which requires OCaml:

    git submodule update external/damsl-k3
    (cd external/damsl-k3; make)

Then fetch, build, and install any dependencies (for the moment, we seem to
be doing OK with vanilla upstreams!)

    make deps

Build Dyna:

    make build

(If that doesn't work, you might try `make ghcbuild` which ignores the Cabal
infrastructure.)

Run the test harness:

    make tests

And then run the REPL:

    ./dist/build/drepl/drepl

OK, that last bit is probably not quite true.  At this point, the code is
still rather "in the works" so you probably want to...

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
