#!/usr/bin/env bash

# The example shows off some of the REPL's data loading commands.

./dyna test/repl/load.dyna \
  --load 'rules_tsv = tsv("test/repl/papa.gr")' \
         'token = matrix("test/repl/sentences.txt", astype=str)' \
         'tree = sexpr("test/repl/trees.txt")' \
  --post 'dump_chart()'
