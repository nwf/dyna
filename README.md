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

Background and Tutorials
------------------------

* [Papers and slides](http://cs.jhu.edu/~jason/papers/#Dyna) about Dyna
* Installation instructions below
* [Hands-on tutorial](http://cs.jhu.edu/~darcey/dyna-tutorial.pdf) to get started 
* [Slides](http://cs.jhu.edu/~jason/licl/) (see 7/5 and 7/11) using Dyna code to teach natural language processing algorithms 
* [Homework exercises](http://cs.jhu.edu/~jason/licl/hw3/hw3.pdf) using this implementation

*Note*: A previous, efficient implementation for a more restricted version of the language can be found at [dyna.org](http://dyna.org), along with discussion from that time.

Quick Start
-----------

First clone the git repository on your local machine, e.g.,
   git clone http://github.com/nwf/dyna
or download a snapshot as a zip file:
   wget https://github.com/nwf/dyna/archive/master.zip

Ensure that you have the Haskell platform 2012.2 or later installed
either through your favorite package manager (e.g., `apt-get install
haskell-platform`) or by installing it stand-alone from the haskell homepage.

This is enough to get you the compiler up and running. But to execute
programs, you'll also need to set up the Python backend. For that
you'll need `Python 2.7+` and the following python modules

    $ easy_install ipython

Optionally, installing the following will enable certain extension

    $ easy_install pygments matplotlib
    $ apt-get install graphviz

Now you're ready to build.

    make

this will build dependencies and the Dyna compiler executable.  Run the test
harness, just to make sure things built and are working:

    make tests

Now you're ready to run a Dyna program. 

* Run the python backend interactively (leave off the "-i" for bulk operation):

        ./dyna -i examples/papa2.dyna

* Work through the tutorial and homework exercises listed above.

* Try other programs in the `examples/` directory, or write your own.

Documentation is being added at

    make sphinxdoc

Authors and Acknowledgments
---------------------------

This implementation was written primarily by Nathaniel Wesley Filardo
and Tim Vieira, advised by Jason Eisner.  Thanks to other contributors
as well, particularly everyone who worked on the [previous
version](http://dyna.org).  

This work has been supported in part by the Human Language Technology
Center of Excellence and and by the National Science Foundation under
Grant No. 0964681.

Licence and Disclaimer
----------------------

This is work in progress.  It is licensed under the AGPLv3 (available
in LICENSE).  No guarantees of completeness or correctness are made at
this point.  If you're trying to make it do something and it breaks,
you get to keep all the pieces; see sections 15-17 of the license.
However, please report any unknown issues via the [issue
tracker](https://github.com/nwf/dyna/issues).
