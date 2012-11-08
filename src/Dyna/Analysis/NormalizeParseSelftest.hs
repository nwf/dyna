{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dyna.Analysis.NormalizeParseSelftest where

import qualified Data.ByteString              as B
import           Dyna.Analysis.NormalizeParse
import qualified Dyna.ParserHS.Parser         as P
import           Dyna.Term.TTerm
import           Dyna.XXX.TrifectaTest

testNormTerm :: Monad m => B.ByteString -> (DTerm, ANFState)
testNormTerm = runNormalize . normTerm False . unsafeParse P.dterm

testNormRule :: Monad m => B.ByteString -> (DRule, ANFState)
testNormRule = runNormalize . normRule . unsafeParse P.drule
