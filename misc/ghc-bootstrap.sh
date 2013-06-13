#!/bin/bash

# This script is useful for bringing up a Haskell world on a machine which
# doesn't have one (use the ghc tarballs) or to bring one up from source.
# Written because I was tracking ghc HEAD for a while, but stuck here
# because it's proven itself useful to bring up a Dyna-capable environment
# on a few machines.


# NOTICE!  YOU MUST UPDATE THIS LINE!
#
# This is an absolute path devoid of $HOME so that this file
# continues to work when sourced by people whose homedirs are not mine!
HROOT=~nwf/src/haskell

GHCVER=7.6.3

HTMLROOT="../../"

CABAL_BOOTSTRAP_PKGS="transformers-0.3.0.0 \
         mtl-2.1.2 \
         text-0.11.2.3 \
         zlib-0.5.4.0 \
         parsec-3.1.3 \
         network-2.4.1.0 \
         random-1.0.1.1 \
         HTTP-4000.2.6 \
         cabal-install-1.16.0.2"

HPATH=$HROOT/_inst
HBIN=$HPATH/bin
HLIB=$HPATH/lib/ghc-$GHCVER

export CABALOPTS_BOOTSTRAP="--prefix=$HPATH --global --enable-library-profiling"
export CABALOPTS="--prefix=$HPATH --global --enable-library-profiling \
                  --haddock-html --haddock-html-location=${HTMLROOT}/\$pkgid/html/"

export PATH=$HBIN:$PATH

# Enable some hackery around library files being misnamed
export LD_LIBRARY_PATH=$HLIB${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

CMD=$1;
shift;

case $CMD in
    install)
    $HBIN/cabal install $CABALOPTS --global $@;
    ;;

    buildpwd)
    $HBIN/runghc Setup configure $CABALOPTS --global $@ &&
    $HBIN/runghc Setup build &&
    $HBIN/runghc Setup install
    ;;

    printpaths)
    echo export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
    echo export PATH=$PATH
    ;;

    run)
    export PREFIX=$HPATH
    "$@"
    ;;

    reinstall)
    export PREFIX=$HPATH
    cd $HROOT
    rm -rf _inst &&
    (cd ghc-$GHCVER;
        (./boot || exit 0) &&
        ./configure --prefix=$PREFIX &&
        if [ -x ./boot]; then make -j 12 all; fi ) && 
    (cd ghc-$GHCVER; make install) &&
        # Hack around Debian's foolishness
    (ln -s /usr/lib/libgmp.so.? _inst/lib/ghc-$GHCVER/libgmp.so) &&
        # Bootstrap cabal-install
    (cd pkgs; for i in $CABAL_BOOTSTRAP_PKGS ;
      do (cd $i; rm -rf dist/;
                 $HBIN/runghc Setup configure $CABALOPTS_BOOTSTRAP;
                 $HBIN/runghc Setup build;
                 $HBIN/runghc Setup install) || exit 1
      done) &&
        # Now, reinstall so that we get documentation
    (cd pkgs; for i in $CABAL_BOOTSTRAP_PKGS;
      do (cd $i; $HBIN/cabal install $CABALOPTS --force-reinstalls); done) &&
        # And last, build everything else we need out of cabal
    $HBIN/cabal update &&
        # Pull in alex and happy first, since we want them for builds below
    $HBIN/cabal install $CABALOPTS -j12 alex happy &&
        # Pull in the packages on which Dyna will depend as well as 
    $HBIN/cabal install $CABALOPTS -j12 \
        blaze-builder \
        blaze-html \
        charset \
        criterion \
        either \
        fingertree \
        fingertree-psqueue \
        haskeline \
        HUnit \
        keys \
        lens \
        MonadCatchIO-transformers \
        pandoc \
        recursion-schemes \
        reducers \
        'semigroups>=0.9' \
        tagged \
        terminfo \
        trifecta \
        test-framework \
        test-framework-golden \
        test-framework-hunit \
        test-framework-program \
        test-framework-quickcheck2 \
        test-framework-smallcheck \
        test-framework-th \
        unordered-containers \
        unification-fd \
        utf8-string \
        wl-pprint-extras \
        wl-pprint-terminfo &&
        # Link in some documentation from the GHC-bundled libraries just so we have a complete list
    (cd _inst/share/doc; find ghc/html/libraries/ -maxdepth 1 -type d -exec ln -s {} . \; -exec ln -s . {}/html \;)
    ;;   

    *)
    echo "I'm sorry, Dave?"
    ;;
esac
