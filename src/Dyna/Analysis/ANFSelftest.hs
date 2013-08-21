{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


--------------------------------------------------------------------------------
-- Don't forget to run the following command at the ghci promt
--    ghci> :set -XOverloadedStrings
--

module Dyna.Analysis.ANFSelftest where


import qualified Data.ByteString              as B
import qualified Data.List                    as L
import qualified Data.Map                     as M
import           Data.Monoid
import qualified Text.Trifecta                as T
import           Text.PrettyPrint.Free

import           Dyna.Analysis.ANF
import           Dyna.Analysis.ANFPretty
import qualified Dyna.ParserHS.OneshotDriver  as PD
import qualified Dyna.ParserHS.Syntax         as P
import qualified Dyna.ParserHS.SyntaxTheory   as P
import qualified Dyna.ParserHS.Parser         as P
import           Dyna.ParserHS.Selftest
import           Dyna.Term.Normalized
import           Dyna.Term.TTerm
import           Dyna.XXX.TrifectaTest


testNormRule :: B.ByteString -> (Rule, ANFWarns)
testNormRule s = normRule ( 0
                          , dt
                          , unsafeParse (P.testRule dlc) s)
 where
  dt = P.mkDisposTab P.defDispos_dyna mempty
  dlc = P.mkDLC P.dynaOperSpec

{-
e1 = testNormRule "f(X)."
e2 = testNormRule "f(X) := 1."

t1 = testNormRule "f(X) max= g(X) + h(X,X)"
t2 = testNormRule "f(X, g(I)) += (g(I, h(X)) + 10)^2"

e4 = "fib(X+1) += fib(X) * fib(X-1) := 1 .\nfib(a) := 1 ."
t4 = unsafeParse P.dlines e4

-- hideous monster rule
e3 = "f(X,Y) += (g(X,\"str\",d) - h(X,X,Y) - c)^2 + f(Y,Z)/exp(3.0) whenever ?c, (d < 10), e(f(h(X)), g(X))"
t3 = testNormRule e3
p3 = printANF $ t3
-}
