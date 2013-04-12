{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.Mode.Selftest.Term where

import           Control.Applicative
import           Control.Monad
import qualified Data.List                 as L
import qualified Data.Map                  as M
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Test.QuickCheck

-- XXX orphan instance
instance Arbitrary Uniq where arbitrary = arbitraryBoundedEnum

data TestFunct = F | G | H
 deriving (Bounded,Enum,Eq,Ord,Show)

instance Arbitrary TestFunct where arbitrary = arbitraryBoundedEnum

arbFuncMap :: forall i . Gen i -> Gen (M.Map TestFunct [i])
arbFuncMap gi = do
    fs :: [TestFunct] <- arbitrary
    foldM (\m f -> genArgs f >>= return . flip (M.insert f) m) M.empty (L.nub fs)
 where
  genArgs :: TestFunct -> Gen [i]
  genArgs F = pure []
  genArgs G = vectorOf 1 gi
  genArgs H = vectorOf 2 gi

arbInstPly :: forall i . Gen i -> Gen (InstF TestFunct i)
arbInstPly n = frequency [ (1,pure IFree)
                         , (1,IAny   <$> arbitrary)
                         , (1,IUniv  <$> arbitrary)
                         , (3,IBound <$> arbitrary
                                     <*> sized (\s -> resize (s`div`2)
                                                      $ arbFuncMap n)
                                     <*> arbitrary)
                         ]


