---------------------------------------------------------------------------
-- | Examples of the Automata library's use
--
-- Used as a sanity check on the design of the API. @:)@

-- Header material                                                      {{{
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Dyna.XXX.Automata.Examples where

import           Control.Applicative
import           Control.Monad (join)
import           Data.Functor.Foldable (Fix(..))
import qualified Data.Foldable                     as F
import qualified Data.Map                          as M
import qualified Data.Traversable                  as T
import           Dyna.XXX.Automata.RecClass
import           Dyna.XXX.Automata.ReprClass
import qualified Dyna.XXX.Automata.FixAut          as FA
import qualified Dyna.XXX.Automata.NamedAut        as NA
import qualified Prelude.Extras                    as PE
import           Test.Framework                    as TF
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import qualified Text.PrettyPrint.Free             as PP

------------------------------------------------------------------------}}}
-- Deterministic string automata                                        {{{
-- Definition                                                           {{{
-- | DFA functor: each state is either accepting or rejecting and has a
-- deterministic transition, keyed by emission symbol, to a successor state.
-- Missing the map is interpreted as immediately failing, as per usual
-- representations.
--
-- The root state is handled by the automata representation and is not
-- visible in the machinery here.

data DFAF alphabet state = DFAF Bool (M.Map alphabet state)
 deriving (Eq, Functor, F.Foldable, Ord, Show, T.Traversable)

instance Eq a  => PE.Eq1  (DFAF a)
instance Ord a => PE.Ord1 (DFAF a)

renderDFAF :: (PP.Pretty alphabet) => DFAF alphabet (PP.Doc e) -> PP.Doc e
renderDFAF (DFAF b m) = PP.parens
                      $ PP.pretty b
                        PP.<> ","
                        PP.<+> PP.semiBraces
                                (map (\(k,v) -> (PP.pretty k) PP.<+> "->" PP.<+> v)
                                     (M.toList m))

instance AutStructEmpty (DFAF alphabet) where
 autsEmpty = \case
   DFAF True  _ -> AEHasFinite
   DFAF False t -> foldr max AEEmpty $ F.toList t

instance (Ord alphabet) => AutStructInter (DFAF alphabet) where
 autsInter = AutMerge (\_ _ -> ())
                      (error "Intersection should not call import")
                      go
  where
   go _ _ _ _ merge _ (DFAF lb lm) (DFAF rb rm) =
    DFAF (lb && rb) <$> 
      (T.sequenceA $ M.intersectionWith (\lk rk -> merge () lk rk) lm rm)

instance (Ord alphabet) => AutStructUnion (DFAF alphabet) where
 autsUnion = AutMerge (\_ _ -> ()) goi gor
  where
   goi r _ (DFAF f m) = DFAF f <$> (T.sequenceA (r () <$> m))

   gor li ri _ _ merge _ (DFAF lb lm) (DFAF rb rm) =
    DFAF (lb || rb) <$>
      (T.sequenceA $ M.mergeWithKey (\_ lk rk -> Just $ merge () lk rk)
                                    (M.map (li ())) (M.map (ri ()))
                                    lm rm)

------------------------------------------------------------------------}}}
-- Examples                                                             {{{

dfa_empty             = nafl 0 [ (0, DFAF False M.empty) ]
dfa_only_empty_string = nafl 0 [ (0, DFAF True  M.empty) ]

dfa_sigma_star :: (Bounded a, Enum a, Ord a) => NA.NA (DFAF a)
dfa_sigma_star        = nafl 0 [ (0, DFAF True
                                   (M.fromList $ map (\x -> (x,0))
                                                     (enumFrom minBound))) ]


dfa_even_falses, dfa_even_trues :: NA.NA (DFAF Bool)
dfa_even_falses = nafl 0 [ (0, DFAF True  (M.fromList [(False,1), (True,0)]))
                         , (1, DFAF False (M.fromList [(False,0), (True,1)]))
                         ]

trans_count_trues = [ (0, DFAF True  (M.fromList [(True,1), (False,0)]))
                    , (1, DFAF False (M.fromList [(True,0), (False,1)]))
                    ]

dfa_even_trues = nafl 0 trans_count_trues
dfa_odd_trues  = nafl 1 trans_count_trues

-- | Accepts "True" or "False False" and that's it.
dfa_simple_finite = nafl 0 [ (0, DFAF False (M.fromList [(True,1), (False,2)]))
                           , (1, DFAF True  M.empty)
                           , (2, DFAF False (M.fromList [(False,1)]))
                           ]

-- | This one is a little funny: it has unfounded recursion, which typically
--   poses a problem for emptyness testing
dfa_only_infinite_units :: NA.NA (DFAF ())
dfa_only_infinite_units = nafl 0 [ (0, DFAF False (M.fromList [((),0)])) ]

-- | The union of unfounded recursion and something finite
dfa_finite_among_infinite :: NA.NA (DFAF Bool)
dfa_finite_among_infinite = nafl 0 [ (0, DFAF False (M.fromList [(False,0),(True,1)]))
                                   , (1, DFAF True  (M.fromList [(False,1),(True,1)]))
                                   ]

fa_dfa_empty_string :: Ord a => FA.FA (DFAF a)
fa_dfa_empty_string = autHide (DFAF True M.empty)

fa_dfa_one_false :: FA.FA (DFAF Bool)
fa_dfa_one_false = autHide $ DFAF False $ M.singleton False $
                   autHide $ DFAF True  $ M.empty

fa_dfa_two_trues :: FA.FA (DFAF Bool)
fa_dfa_two_trues = autHide $ DFAF False $ M.singleton True $
                   autHide $ DFAF False $ M.singleton True $
                   autHide $ DFAF True  $ M.empty

fa_dfa_two_falses :: FA.FA (DFAF Bool)
fa_dfa_two_falses = autHide $ DFAF False $ M.singleton False $
                    autHide $ DFAF False $ M.singleton False $
                    autHide $ DFAF True  $ M.empty


------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Nondeterministic string automata                                     {{{

-- | NFA functor: each (state,symbol) pair transitions to a set of possible
-- successor states.  This formulation does not allow for \epsilon edges but
-- requires that we compute epsilon closure of the transition relation.
newtype NFAF alphabet state = NFAF (M.Map alphabet [state])
 deriving (Eq, Functor, F.Foldable, T.Traversable, Show)

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

nafl :: (T.Traversable f) => Int -> [(Int, f Int)] -> NA.NA f
nafl x = flip NA.naFromMap x . M.fromList

------------------------------------------------------------------------}}}
-- Test harness wiring                                                  {{{

case_dfa_empty_empty :: Assertion
case_dfa_empty_empty = assert $ autEmpty dfa_empty

case_dfa_unfounded_empty :: Assertion
case_dfa_unfounded_empty = assert $ autEmpty dfa_only_infinite_units

case_dfa_sigma_star_nonempty :: Assertion
case_dfa_sigma_star_nonempty = assert $ not $ autEmpty (dfa_sigma_star :: NA.NA (DFAF Bool))

case_dfa_finite_among_nonempty :: Assertion
case_dfa_finite_among_nonempty = assert $ not $ autEmpty dfa_finite_among_infinite

accepts a1 a2 = not $ autEmpty (autInter a1 a2)

case_sigma_star_accept_two_trues :: Assertion
case_sigma_star_accept_two_trues = assert $ accepts dfa_sigma_star fa_dfa_two_trues

even_either = autUnion dfa_even_trues dfa_even_falses

case_even_either_accepts_two_trues :: Assertion
case_even_either_accepts_two_trues = assert $ accepts even_either fa_dfa_two_trues

case_even_either_accepts_two_falses :: Assertion
case_even_either_accepts_two_falses = assert $ accepts even_either fa_dfa_two_trues

-- | This test case may seem a little strange.  It's here to demonstrate the
-- difference between the use of the \"import\" callbacks and the
-- \"lop-sided\" callbacks in 'autsUnion'; using the latter (which is
-- incorrect) produces an automaton that mistakenly accepts [False]
-- when it should only accept [], [True], and [False,False].
case_union_imports_not_lops :: Assertion
case_union_imports_not_lops = assert $ not $ accepts
   (dfa_simple_finite `autUnion` dfa_only_empty_string) fa_dfa_one_false

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = TF.defaultMain [selftest]

------------------------------------------------------------------------}}}
