---------------------------------------------------------------------------
-- | Test generators for Inst reasoning.

--   Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.Mode.Selftest.Term where

import           Control.Applicative
import           Control.Monad
import qualified Data.Either               as E
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Maybe                as MA
import qualified Data.Set                  as S
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.XXX.Automata.ReprClass
import           Dyna.XXX.Automata.Utilities
import           Test.QuickCheck
import           Test.SmallCheck           as SC
import           Test.SmallCheck.Series    as SCS
import qualified Text.PrettyPrint.Free     as PP

------------------------------------------------------------------------}}}
-- Misc                                                                 {{{

-- XXX Is this in the library anywhere?
fe :: (Bounded a, Enum a) => [a]
fe = enumFrom minBound

-- XXX Goes upstream to quickcheck?
arbitrarySetBEO :: forall a . (Bounded a, Enum a, Ord a) => Gen (S.Set a)
arbitrarySetBEO = do
  pres <- mapM (\_ -> arbitrary) (fe :: [a])
  let mf = zipWith (\p f -> if p then Just f else Nothing) pres fe
  return $ S.fromList $ MA.catMaybes mf

-- XXX Goes upstream to smallcheck?
serBoundedEnum :: forall a m . (Bounded a, Enum a, Monad m) => Series m a
serBoundedEnum = foldr1 (\/) $ map cons0 fe

-- XXX Goes upstream to smallcheck?
serSetBEO :: forall a m . (Bounded a, Enum a, Ord a, Monad m) => Series m (S.Set a)
serSetBEO = do
  -- 'liftM not' needed so that we generate smaller sets first
  pres <- mapM (\_ -> liftM not series) (fe :: [a])
  let mf = zipWith (\p f -> if p then Just f else Nothing) pres fe
  return $ S.fromList $ MA.catMaybes mf

------------------------------------------------------------------------}}}
-- Uniq                                                                 {{{

-- XXX orphan instance
instance Arbitrary Uniq where arbitrary = arbitraryBoundedEnum

-- XXX orphan instance
instance (Monad m) => Serial m Uniq where series = serBoundedEnum

arbUniq :: Uniq -> Gen Uniq
arbUniq u = oneof $ map return [u..]

------------------------------------------------------------------------}}}
-- Closed-world functor                                                 {{{

-- | The convention here is that these are F/0, G/1, H/2.
data TestFunct = F | G | H
 deriving (Bounded,Enum,Eq,Ord,Show)

instance PP.Pretty TestFunct where pretty = PP.text . show

genArgs :: (Monad m) => m i -> TestFunct -> m [i]
genArgs _ F = return []
genArgs g G = g >>= return . (\x -> [x])
genArgs g H = do { i <- g; j <- g; return [i,j] }

genFuncMap :: forall i m . (Monad m)
           => m (S.Set TestFunct) -> m i -> m (M.Map TestFunct [i])
genFuncMap stf gi = do
	-- 'liftM S.toList' keeps us from generating lists containing
	-- the same functor more than once.
    fs :: [TestFunct] <- liftM S.toList stf
    foldM (\m f -> genArgs gi f >>= return . flip (M.insert f) m)
          M.empty
          (L.nub fs)

------------------------------------------------------------------------}}}
-- Term: QuickCheck Generators                                          {{{

instance Arbitrary TestFunct where arbitrary = arbitraryBoundedEnum
instance Arbitrary (S.Set TestFunct) where arbitrary = arbitrarySetBEO

-- | Generate an arbitrary, well-formed NIX at some uniqueness.
arbNIX :: Uniq -> Gen (NIX TestFunct)
arbNIX rootU = do
    n <- sized (\s -> choose (1::Int,max 1 s)) -- How many plies?

    let nl   = [1..n]

    uniqs <- vectorOf n (arbUniq rootU)        -- Generate some uniques
                                               -- that are all plausible
                                               -- recursion candidates

    let spares = zip [minBound.. ] [n+1 ..]

        -- For each recursion site, pick a plausible recursor
    let igen u = oneof $ map (return.snd) $ filter ((== u) . fst)
                       $ (zip uniqs nl ++ spares)

    plies <- mapM (flip arbInstPly igen) uniqs   -- Generate plies or recurse

    let m = M.fromList (zip nl plies            -- Make the map
                        ++ map (\(_,i) -> (i,IFree)) spares)
    root <- choose (1,n)
    return $ nFromMap m root
 where
  arbInstPly :: forall i . Uniq -> (Uniq -> Gen i) -> Gen (InstF TestFunct i)
  arbInstPly u n = frequency
    [ (1,pure IFree)
    , (1,IAny   <$> arbUniq u)
    , (1,IUniv  <$> arbUniq u)
    , (3,do
          u' <- arbUniq u
          IBound <$> pure u'
                 <*> sized (\s -> resize (s`div`2)
                                  $ genFuncMap arbitrary (n u'))
                 <*> arbitrary)
    ]
  
-- | The not-necessarily-well-formed variant
arbNIXNWF :: Gen (NIX TestFunct)
arbNIXNWF = do
    n <- sized (\s -> choose (1::Int,max 1 s)) -- How many plies?
    let nl   = [1..n]
    let igen = choose (1::Int,n)               -- Pick a ply
    plies <- vectorOf n (arbInstPlyNWF igen)   -- Generate plies or recurse
    let m = M.fromList $ zip nl plies          -- Make the map
    root <- choose (1,n)
    return $ nFromMap m root
 where
  arbInstPlyNWF :: forall i . Gen i -> Gen (InstF TestFunct i)
  arbInstPlyNWF n = frequency
    [ (1,pure IFree)
    , (1,IAny   <$> arbitrary)
    , (1,IUniv  <$> arbitrary)
    , (3,IBound <$> arbitrary
                <*> sized (\s -> resize (s`div`2)
                                 $ genFuncMap arbitrary n)
                <*> arbitrary)
    ]

instance Arbitrary (NIX TestFunct) where
  arbitrary = liftM nMinimize $ arbNIX UUnique

{-
  shrink n@(NIX r m) = (MA.maybeToList noRecN) ++ subautomata
   where
    -- Replace all calls out to other automata with references to the root
    noRecN = if null (E.lefts $ M.elems m) then Nothing else Just $ NIX r m'
     where m' = M.map (either (const $ Right r) Right) m

    -- Pull out all the subautomata
    subautomata = E.lefts (M.elems m)
-}

------------------------------------------------------------------------}}}
-- Term: SmallCheck Generators                                          {{{

-- XXX These are buggy (they generate infinite, or at least "certainly too
-- long" lists)

{-
instance (Monad m) => Serial m TestFunct where series = serBoundedEnum
instance (Monad m) => Serial m (S.Set TestFunct) where series = serSetBEO

serInstPly :: forall i m . (Monad m)
           => Series m i
           -> Series m (InstF TestFunct i)
serInstPly n =    cons0 IFree
               \/ cons1 IAny
               \/ cons1 IUniv
               \/ (IBound <$> series
                          <*> genFuncMap series n
                          <*> series)

serNIXME n = cons1 Left \/ (Right <$> serInstPly n)

instance (Monad m) => Serial m (NIX TestFunct) where
  series = do
    SCS.Positive (n :: Int) <- series
    let nl = [1..n]
    let igen = generate (const nl)
    plies <- mapM (\_ -> serNIXME igen) nl
    let m = M.fromList $ zip nl plies
    root <- serInstPly igen
    return $ NIX root m
-}

------------------------------------------------------------------------}}}
