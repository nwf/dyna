#!/usr/bin/env bash

# The example shows off some of the REPL's data loading commands.

echo '
load rules_tsv = tsv("test/repl/papa.gr").
load token = loadmat("test/repl/sentences.txt", astype=str).
' |./dyna test/repl/load.dyna -i

#load tree_sexpr = sexpr("trees.txt", binarize=True)
