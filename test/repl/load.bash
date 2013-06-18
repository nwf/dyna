#!/usr/bin/env bash

# The example shows off some of the REPL's data loading commands.

./dyna test/repl/load.dyna \
  --load 'rules_tsv = tsv("test/repl/english.gr")' \
         'token = matrix("test/repl/english.sen", astype=str)' \
         'tree = sexpr("test/repl/english.par")' \
  --post 'dump_chart()' \
  "$@"