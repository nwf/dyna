{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.Mode.Selftest.Term where

import           Control.Applicative
import           Control.Monad
import qualified Data.Either               as E
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Maybe                as MA
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Analysis.Mode.Execution.NamedInst
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

-- | Generate an Arbitrary 'NIXM' Entry (i.e. either a closed NIX automata
-- or a ply)
arbNIXME :: forall f i . (f ~ TestFunct)
         => Gen i -> Gen (Either (NIX f) (InstF f i))
arbNIXME igen = oneof [Left  <$> sized (\s -> resize (s`div`2) arbitrary)
                      ,Right <$> arbInstPly igen]

arbNIX = do
    n <- sized (\s -> choose (1::Int,max 1 s)) -- How many plies?
    let nl   = [1..n]
    let igen = choose (1::Int,n)               -- Pick a ply
    plies <- vectorOf n (arbNIXME igen)        -- Generate plies or recurse
    let m = M.fromList $ zip nl plies          -- Make the map
    root <- arbInstPly igen                    -- The root
    return $ NIX root m

instance Arbitrary (NIX TestFunct) where
  arbitrary = nPrune <$> arbNIX

  shrink n@(NIX r m) = (MA.maybeToList noRecN) ++ subautomata
   where
    -- Replace all calls out to other automata with references to the root
    noRecN = if null (E.lefts $ M.elems m) then Nothing else Just $ NIX r m'
     where m' = M.map (either (const $ Right r) Right) m

    -- Pull out all the subautomata
    subautomata = E.lefts (M.elems m)

