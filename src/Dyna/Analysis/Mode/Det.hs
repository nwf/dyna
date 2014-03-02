---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system (Determinism)
--
-- This module contains the definitions of determinism and
-- primitive predicates defined by the thesis.

-- Header material                                                      {{{
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode.Det(Det(..), detLt, detLe, detMax) where
------------------------------------------------------------------------}}}
-- Determinism                                                          {{{

-- | Mercury determinism (Table 2.2, p24)
--
-- The 'Ord' instance is solely for internal use; for reasoning, use lattice
-- functions.  The 'Bounded' and 'Enum' instances are for the test suite's
-- use.
data Det = DetErroneous     -- ^ At most zero  results, and yet cannot fail
         | DetFail          -- ^ Exactly zero  results
         | Det              -- ^ Exactly one   result
         | DetSemi          -- ^ Zero or one   result
         | DetMulti         -- ^ At least one  result
         | DetNon           -- ^ At least zero results
 deriving (Bounded, Eq, Enum, Ord, Show)

-- | Determinism lattice transitive, non-reflexive partial ordering function
--
-- Figure 2.4, p25
detLt :: Det -> Det -> Bool
detLt DetErroneous b          = b /= DetErroneous
detLt DetFail      DetSemi    = True
detLt DetFail      DetNon     = True
detLt Det          DetSemi    = True
detLt Det          DetMulti   = True
detLt Det          DetNon     = True
detLt DetSemi      DetNon     = True
detLt DetMulti     DetNon     = True
detLt _            _          = False
{-# INLINABLE detLt #-}

-- | Determinism lattice transitive, reflexive partial ordering function
detLe :: Det -> Det -> Bool
detLe a b | a == b = True
detLe a b          = detLt a b
{-# INLINABLE detLe #-}

detMax :: Det -> Det -> Det
detMax DetErroneous x            = x
detMax x            DetErroneous = x

detMax DetFail      Det          = DetSemi
detMax Det          DetFail      = DetSemi
detMax DetFail      DetMulti     = DetNon
detMax DetMulti     DetFail      = DetNon
detMax DetFail      x            = x
detMax x            DetFail      = x

detMax Det          Det          = Det
detMax Det          DetSemi      = DetSemi
detMax DetSemi      Det          = DetSemi
detMax DetSemi      DetSemi      = DetSemi
detMax Det          DetMulti     = DetMulti
detMax DetMulti     Det          = DetMulti

detMax DetSemi      DetMulti     = DetNon
detMax DetMulti     DetSemi      = DetNon

detMax DetMulti     x            = x
detMax x            DetMulti     = x

detMax DetNon       _            = DetNon
detMax _            DetNon       = DetNon

-- test_detMax_LE l r = let m = detMax l r in l `detLe` m && r `detLe` m
-- test_detMax_Symm l r = let (m,n) = (detMax l r, detMax r l) in m == n
--
-- allDets = [(minBound :: Det)..]
-- allDetPairs = allDetPairs = do { l <- allDets; r <- allDets; return (l,r) }
-- bad1 = filter (\(_,_,x) -> not x) $ map (\(l,r) -> (l,r,test_detMax_LE l r)) allDetPairs

------------------------------------------------------------------------}}}
