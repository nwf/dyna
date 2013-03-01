---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system (Determinism)
--
-- This module contains the definitions of determinism and
-- primitive predicates defined by the thesis.

-- Header material                                                      {{{
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode.Det(Det(..), detLt, detLe) where
------------------------------------------------------------------------}}}
-- Determinism                                                          {{{

-- | Mercury determinism (Table 2.2, p24)
--
-- The 'Ord' instance is solely for internal use; for reasoning, use lattice
-- functions.
data Det = DetErroneous     -- ^ At most zero  results, and yet cannot fail
         | DetFailure       -- ^ Exactly zero  results
         | Det              -- ^ Exactly one   result
         | DetSemi          -- ^ Zero or one   result
         | DetMulti         -- ^ At least one  result
         | DetNon           -- ^ At least zero results
 deriving (Eq, Ord, Show)

-- | Determinism lattice transitive, non-reflexive partial ordering function
--
-- Figure 2.4, p25
detLt :: Det -> Det -> Bool
detLt DetErroneous b          = b /= DetErroneous
detLt DetFailure   DetSemi    = True
detLt DetFailure   DetNon     = True
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

------------------------------------------------------------------------}}}
