# -*-  indent-tabs-mode:t;  -*-

VERFILE=dist/VERSION	# XREF:VERSION

all: deps build sphinxbuild

.PHONY: $(VERFILE)
$(VERFILE):
	mkdir -p `dirname $@`
	echo "Version: Dyna 0.4 pre-release" `git describe --all` > $@
	echo "Build date:" `date` >> $@
	echo "Commit: https://github.com/nwf/dyna/commit/"`git rev-parse HEAD` >> $@

upstream:
	git submodule init
	# git submodule update external/ekmett-parsers external/ekmett-trifecta
	# cabal install --user --enable-tests --only-dependencies \
	#  external/ekmett-parsers external/ekmett-trifecta .
	# cabal install --user external/ekmett-parsers external/ekmett-trifecta

deps:
	alex --version 2>/dev/null >/dev/null || cabal install alex
	happy --version 2>/dev/null >/dev/null || cabal install happy
	cabal install --user --enable-tests --only-dependencies .

build: $(VERFILE)
	cabal configure --user
	cabal build

test tests:
	cabal configure --user --enable-tests
	cabal build
	dist/build/dyna-selftests/dyna-selftests
	misc/dyna-doctest.py
	# cabal test

# Compilation takes a while; for faster iteration while developing,
# turn off optimization.
fcomp:
	cabal configure --user --enable-tests -O0
	cabal build

.PHONY: clean veryclean
clean:
	rm -rf examples/*.dyna.*.plan  \
	   examples/*.dyna.*.planc \
	   examples/*.dyna.plan.py \
	   examples/*.dyna.plan.pyc \
	   examples/*.dyna.*.out \
	   examples/*.dyna.d \
	   examples/*.hist
	rm -rf test/*/*.out
	rm -rf *.tix
	rm -f tags TAGS
veryclean: clean
	rm -rf dist/*

# Cabal's haddock integration is sort of sad; since I want to have
# everything we use in one place, run haddock by hand.  This still isn't
# perfect, but it does OK.

HADDOCK_HTML ?= "../\\$$pkgid/html"
.PHONY: haddock
haddock:
	mkdir -p dist/alldoc
	haddock --html -o dist/alldoc \
	 --ignore-all-exports -w --optghc=-isrc --optghc=-idist/build/autogen \
	 -t "Dyna -- GIT `git describe --always`" \
	 `runghc -imisc HaddockPaths "$(HADDOCK_HTML)"` \
	 `grep -ie '^\( \|\t\)*main-is:' dyna.cabal | sed -e "s/^.*Is: */src\//"`

# Build our sphinx documentation
.PHONY: sphinxbuild sphinxdoc
sphinxbuild:
	which sphinx-build >/dev/null && (cd docs/sphinx; make html)

sphinxdoc: sphinxbuild
	python -c 'import webbrowser; \
	  webbrowser.open("./docs/sphinx/_build/html/index.html")'

doc: sphinxbuild haddock

# If the cabal file doesn't do the right thing, this tries to work through
# it all by hand.  Blech!  But it's better than nothing.
.PHONY: ghcbuild
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
# of the tree.  Set MAINMOD and MAINFILE and make this target.
.PHONY: profbuild
profbuild:
	mkdir -p dist/pb
	ghc --make -O0 -rtsopts \
	    -idist/build/autogen -isrc \
	    -o         dist/pb/a-noprof.out \
	    -outputdir dist/pb \
	    -main-is $(MAINMOD) $(MAINFILE)
	ghc --make -O0 -rtsopts \
	    -idist/build/autogen -isrc \
	    -osuf p.o -hisuf p.hi \
	    -prof -fprof-auto -caf-all -auto-all \
	    -o         dist/pb/a.out \
	    -outputdir dist/pb \
	    -main-is $(MAINMOD) $(MAINFILE)

.PHONY: test-hpc
test-hpc:
	mkdir -p dist/selftest-hpc
	ghc -O0 -fhpc -isrc -idist/build/autogen --make \
		-o         dist/selftest-hpc/dyna-selftests     \
		-outputdir dist/selftest-hpc/dyna-selftests-tmp \
		-hpcdir dist/selftest-hpc/hpc \
		-main-is Dyna.Main.TestsDriver Dyna.Main.TestsDriver
	# Remove any old .tix files
	# (prevent "module mismatch with .tix/.mix file hash number")
	rm -rf dyna-selftests.tix
	# Run from top-level
	dist/selftest-hpc/dyna-selftests
	# Then move the mix and tix files out of the way
	mv dyna-selftests.tix dist/selftest-hpc
	# And last, generate markup
	hpc markup \
		--destdir=dist/selftest-hpc-out \
		--hpcdir=dist/selftest-hpc/hpc \
		dist/selftest-hpc/dyna-selftests.tix

.PHONY: tags TAGS
tags TAGS:
	hasktags -b src

coverage:
	(coverage run --rcfile misc/coveragerc misc/dyna-doctest.py \
	 ; coverage html --rcfile misc/coveragerc --include 'src/*' -d coverage-report \
	 ; gnome-open coverage-report/index.html)
