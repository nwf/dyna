# -*-  indent-tabs-mode:t;  -*-

all: build

upstream:
	git submodule init
	git submodule update external/ekmett-parsers external/ekmett-trifecta
	cabal install --user --enable-tests --only-dependencies \
      external/ekmett-parsers external/ekmett-trifecta .
	cabal install --user external/ekmett-parsers external/ekmett-trifecta 

deps:
	cabal install --user --enable-tests --only-dependencies .

build:
	cabal configure --user --enable-tests
	cabal build
	cabal test

# If the cabal file doesn't do the right thing, this tries to work through
# it all by hand.  Blech!  But it's better than nothing.
ghcbuild:
	mkdir -p dist/build/dyna/
	mkdir -p dist/build/dyna/dyna-tmp
	ghc --make -isrc                \
		-o         dist/build/dyna/dyna     \
		-outputdir dist/build/dyna/dyna-tmp \
		-main-is Dyna.Main.Driver Dyna.Main.Driver
	
	mkdir -p dist/build/dyna-selftests
	mkdir -p dist/build/dyna-selftests/dyna-selftests-tmp
	ghc --make -isrc                          \
		-o         dist/build/dyna-selftests/dyna-selftests     \
		-outputdir dist/build/dyna-selftests/dyna-selftests-tmp \
		-main-is Dyna.Main.TestsDriver Dyna.Main.TestsDriver

# Every now and again we need to make a profiling build of some component
# of the tree.  Se MAINMOD and MAINFILE and make this target.
profbuild:
	mkdir -p dist/pb
	ghc --make -isrc \
	     -o         dist/pb/a.out \
		 -outputdir dist/pb \
		 -main-is $(MAINMOD) $(MAINFILE)
	ghc --make -isrc -osuf p.o -prof -fprof-auto \
	     -o         dist/pb/a.out \
		 -outputdir dist/pb \
		 -main-is $(MAINMOD) $(MAINFILE)

tests:
	dist/build/dyna-selftests/dyna-selftests

run-parser:
	ghci -isrc Dyna.ParserHS.Parser

.PHONY: clean veryclean
clean:
	rm -rf examples/*.dyna.plan  \
           examples/*.dyna.*.out \
           examples/*.dyna.d
	rm -f tags TAGS
veryclean: clean
	rm -rf dist

tags TAGS:
	hasktags -b src
