---------------------------------------------------------------------------
--  | Template haskell for deriving tuple-handling functions
--
--  I dearly wish I didn't have to do this kind of thing
--  (and even though I am, it doesn't quite work!)

-- Header material                                                      {{{

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Dyna.XXX.THTuple(
    -- * Promoted-kind type functions for tuples and rtuples
  MKLT,MKRLT,

    -- * Classes on tuples and rtuples
  Tupled(..),RTupled(..),

    -- * Template Haskell utility for recursive instances
  mkTupleRecInstances,

    -- * Template Haskell utility functions for type-level shifting
  mkTyMap, mkTyMapFlat, mkTyUnMap,

  {-
  mkRecInstances, 
  -}

    -- * Template Haskell utility functions for data-level
  mkLRecInstances
) where

import           Dyna.XXX.HList
import           Dyna.XXX.THTupleInternals
import           GHC.Prim (Constraint)

------------------------------------------------------------------------}}}
-- Type-level functions                                                 {{{

type family   MKLT (x :: [k]) :: k
type instance MKLT '[] = ()
type instance MKLT (a ': '[]) = a
$(mkMKLTs ''MKLT)

type family   MKRLT (r :: k -> k') (x :: [k]) :: k'
type instance MKRLT r '[] = ()
type instance MKRLT r (a ': '[]) = r a
$(mkMKRLTs ''MKRLT)

------------------------------------------------------------------------}}}
-- Exported classes                                                     {{{

-- | Some type algebra on tuples full of constructed types
--   which is invariant over the constructor in question
--
--   e.g. RTER (a,b) r = (r a, r b)
class (MKLT (TOL base) ~ base) => Tupled base where
  -- | Apply r to each element of the tuple
  type RTER base (r :: * -> *) :: *

  -- | Go from the tuple representation to a promoted list;
  --   the inverse of MKLT (as asserted by class constraints).
  type TOL base :: [*]

  -- | Send a tuple to an HList
  tupleHL   :: base -> HList (TOL base)

  -- | Send a HList to a Tuple
  hlTuple   :: HList (TOL base) -> base

  -- | Shed a type constructor
  tupleopR  :: (RTupled rbase, (RTR rbase) ~ r, (RTE rbase) ~ base)
            => (forall x . r x -> x) -> rbase -> base

  -- | Remap a type constructor
  tupleopRS :: (RTupled rbase, (RTR rbase) ~ r, (RTE rbase) ~ base,
                RTupled sbase, (RTR sbase) ~ s, (RTE sbase) ~ base)
            => (forall x . r x -> s x) -> rbase -> sbase

-- | Recover the constructor and base type from r-full tuples.
--
-- e.g. RTR (r a, r b) = r, RTE (r a, r b) = (a, b)
--
-- This class further specifies some equivalence properties
-- on RTER and MKRLT.
class (Tupled (RTE arred),
       RTER (RTE arred) (RTR arred) ~ arred,
       MKRLT (RTR arred) (TOL (RTE arred)) ~ arred
      )
      => RTupled arred where
  type RTR arred :: (* -> *)
  type RTE arred :: *

  -- | Eliminate an rtuple out to a list.
  tupleopEL :: (RTR arred ~ r) => (forall x . r x -> a) -> arred -> [a]

------------------------------------------------------------------------}}}
-- Aaaand action                                                        {{{

  -- Generate instances for Tupled
$(mkTupleInstances
  ''Tupled ''RTER
  ''TOL '(:+) 'HN 'tupleHL 'hlTuple
  'tupleopR 'tupleopRS)

  -- Generate instances for RTupled
$(mkRTupleInstances ''RTupled ''RTE ''RTR 'tupleopEL)
------------------------------------------------------------------------}}}

