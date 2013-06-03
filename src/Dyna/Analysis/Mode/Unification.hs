---------------------------------------------------------------------------
-- | Wrappers around 'InstF' primitives that are useful during unification.

-- Header material                                                      {{{
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Unification {-(
)-} where

import           Control.Lens.TH
import qualified Data.Maybe                        as MA
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
-- import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Unification Failure Flavors                                          {{{

data UnifFail =
    UFSemiClob -- ^ see 'semidet_clobbered_unify'
  | UFNotReach -- ^ Some nested unification satisfies 'iNotReached'
  | UFExDomain -- ^ A partial function was applied outside its domain
 deriving (Eq,Ord,Show)

------------------------------------------------------------------------}}}
-- Unification-wide Read-only Parameters                                {{{

data UnifParams = UnifParams
                { _up_live :: Bool
                    -- ^ Are we engaged in a live unification?  See §3.2.1,
                    -- p43 and definition 3.2.19, p53

                , _up_fake :: Bool
                    -- ^ Absent from the thesis but present in the Mercury
                    -- implementation is the consideration of "fake"
                    -- unifications, which are used when refining the
                    -- outputs of method calls and must be allowed to
                    -- descend through (Mostly)'Clobbered' material.
                    --
                    -- See @compiler/prog_data.m@'s @unify_is_real@ type.
                }
$(makeLenses ''UnifParams)

------------------------------------------------------------------------}}}
-- Unification                                                          {{{

-- | This predicate is used to ensure that we reject any attempt at
-- unification which could fail (i.e. is semidet, or, possibly better
-- phrased, must traverse the structure of its argument) and may attempt
-- to read clobbered state.
--
-- In words, a unification can enter its arguments whenever
--     1. both inputs are not free variables (a free variable turns
--        unification into assignment; two makes it aliasing)
--     2. either input represents more than one possible term
--
-- The thesis will invoke this function (or rather, its negation) to allow a
-- /dead/ unification (i.e., one in which one of the two variables is not
-- live going forward) to succeed.  Live unifications are probably (yes?
-- XXX?) permitted because it's always possible (if unlikely) that some
-- predicate can run with a clobbered input, and if not, we'll fail at that
-- point.  A semidet unification, on the other hand, cannot run with a
-- clobbered input.
--
-- Definition 3.2.19, p53
--
-- XXX In contrast to the thesis, we ignore the size of the sets represented
-- by the insts we are given, which makes this test wider, and therefore the
-- set of unifications we will accept smaller.
--
semidet_clobbered_unify :: (Monad m) => InstF f i -> InstF f i' -> m Bool
semidet_clobbered_unify i i' = return $
     (not $ iIsFree i)
  && (not $ iIsFree i')
  && (   UMostlyClobbered <= MA.fromJust (iUniq i )
      || UMostlyClobbered <= MA.fromJust (iUniq i'))
    -- The above fromJust calls are safe due to the 'iIsFree' guards.
{-# INLINABLE semidet_clobbered_unify #-}

iLeqGLBRD_,iLeqGLBRL_ :: (Monad m, Ord f)
                      => TyILeqGLB_ f m i i' o (m (Either UnifFail (InstF f o)))
iLeqGLBRD_ il ir ml mr m u i1 i2 = do
    io <- iLeqGLB_ il ir ml mr m u i1 i2
    return $ if iIsNotReached io
              then Left  UFNotReach
              else Right io
{-# INLINABLE iLeqGLBRD_ #-}
iLeqGLBRL_ il ir ml mr m u i1 i2 = do
    scu <- semidet_clobbered_unify i1 i2
    if scu
     then return (Left UFSemiClob)
     else iLeqGLBRD_ il ir ml mr m u i1 i2
{-# INLINABLE iLeqGLBRL_ #-}

------------------------------------------------------------------------}}}
