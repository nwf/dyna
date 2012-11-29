# -*-  indent-tabs-mode:t;  -*-

all: deps

upstream:
	git submodule init
	git submodule update external/ekmett-parsers external/ekmett-trifecta
	(cd external/ekmett-parsers; cabal install --user)
	(cd external/ekmett-trifecta; cabal install --user)

deps:
	(cabal install --enable-tests --only-dependencies)

build: deps
	cabal configure --user --enable-tests
	cabal build
	cabal test

run-parser:
	ghci -isrc Dyna.ParserHS.Parser
