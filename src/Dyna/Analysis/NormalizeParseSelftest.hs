{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


--------------------------------------------------------------------------------
-- Don't forget to run the following command at the ghci promt
--    ghci> :set -XOverloadedStrings
--

module Dyna.Analysis.NormalizeParseSelftest where

import Text.PrettyPrint

import qualified Data.ByteString              as B
import           Dyna.Analysis.NormalizeParse

import qualified Dyna.ParserHS.Parser         as P
import qualified Data.ByteString              as B


import Dyna.Term.TTerm

import           Dyna.XXX.TrifectaTest
import           Dyna.ParserHS.Selftest

import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Text.Trifecta as T

testNormTerm :: B.ByteString -> (DTerm, ANFState)
testNormTerm = runNormalize . normTerm False . unsafeParse P.dterm

testNormRule :: B.ByteString -> (DRule, ANFState)
testNormRule = runNormalize . normRule . unsafeParse P.drule


-- XXX fix periods, parser thinks it's an infix op and fails.
e1 = testNormRule "f(X)."
e2 = testNormRule "f(X) := 1." -- does not work

t1 = testNormRule "f(X) max= g(X) + h(X,X)"
t2 = testNormRule "f(X, g(I)) += (g(I, h(X)) + 10)^2"

e4 = "fib(X+1) += fib(X) * fib(X-1) := 1 .\nfib(a) := 1 ."
t4 = unsafeParse P.dlines e4

-- hideous monster rule
e3 = "f(X,Y) += (g(X,\"str\",d) - h(X,X,Y) - c)^2 + f(Y,Z)/exp(3.0) whenever ?c, (d < 10), e(f(h(X)), g(X))"
t3 = testNormRule e3
p3 = pp $ t3


normalizeFile file = do
    contents <- B.readFile file
    writeFile (file ++ ".anf")
              (show $ vcat (map (\(P.LRule x T.:~ _) -> pp $ runNormalize $ normRule x)
                                (unsafeParse P.dlines contents))
                      <> text "\n") -- add newline at end of file...
    return ()
