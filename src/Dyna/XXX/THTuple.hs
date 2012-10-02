---------------------------------------------------------------------------
--  | Template haskell for deriving tuple-handling functions
--
--  I dearly wish I didn't have to do this kind of thing
--  (and even though I am, it doesn't quite work!)

-- Header material                                                      {{{

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dyna.XXX.THTuple(Tupled(..),RTupled(..),mkRecClass
                        ) where

import          Dyna.XXX.THTupleInternals

------------------------------------------------------------------------}}}
-- Exported classes                                                     {{{

  -- | Some type algebra on tuples full of constructed types
  --   which is invariant over the constructor in question
  --
  --   e.g. RTER (a,b) r = (r a, r b)
class Tupled base where
    -- | Apply r to each element of the tuple
  type RTER base (r :: * -> *) :: *

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
class (Tupled (RTE arred),
       RTER (RTE arred) (RTR arred) ~ arred)
      => RTupled arred where
  type RTR arred :: (* -> *)
  type RTE arred :: *

    -- | Eliminate an rtuple out to a list.
  tupleopEL :: (RTR arred ~ r) => (forall x . r x -> a) -> arred -> [a]

------------------------------------------------------------------------}}}
-- Aaaand action                                                        {{{

  -- Generate instances for Tupled
$(mkTupleInstances ''Tupled ''RTER 'tupleopR 'tupleopRS)

  -- Generate instances for RTupled
$(mkRTupleInstances ''RTupled ''RTE ''RTR 'tupleopEL)
------------------------------------------------------------------------}}}

