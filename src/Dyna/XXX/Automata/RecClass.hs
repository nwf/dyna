---------------------------------------------------------------------------
-- | Standard operations for automata ply functors
--
-- Sometimes, there's really only one set of operations we'd like to support
-- on the structure @f@.  In that case, we can specify the recursors all at
-- once and have a simpler library of operations which use the appropriate
-- recursor.  This simpler library will have things like 'union',
-- 'intersection', 'isEmpty', 'isUniversal', etc. 

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.XXX.Automata.RecClass where

import           Control.Applicative (Applicative)
import qualified Data.Traversable                  as T
import           Dyna.XXX.Automata.ReprClass
import qualified Prelude.Extras                    as PE

------------------------------------------------------------------------}}}
-- Functor recursor class definition                                    {{{

-- | Minimal description of inputs accepted by an automaton.
--
-- The order here is intended for use with `max` as the merge operator.
data AEmpty = AEEmpty         -- ^ No inputs accepted
            | AEInfiniteOnly  -- ^ Unfounded recursion
            | AEHasFinite     -- ^ There exists a finite acceptable input
 deriving (Eq,Ord,Show)

class (T.Traversable f) => AutStructEmpty f where
  autsEmpty :: f AEmpty -> AEmpty

autEmpty :: (AutStructEmpty f, AutomataRepr a) => a f -> Bool
autEmpty =  (/= AEHasFinite) . autReduce AEInfiniteOnly autsEmpty

data AutBiReduce f r = AutBiReduce
  { autBiReduce_cyc :: r
  , autBiReduce_rec :: forall x y m . (Monad m)
                    => (x -> m r)
                    -> (y -> m r)
                    -> (x -> y -> m r)
                    -> f x -> f y -> m r
  }

class (T.Traversable f, PE.Ord1 f) => AutStructSub f where
  autsSub :: AutBiReduce f Bool

autSub  :: (AutStructSub f, AutomataReprBin a a') => a f -> a' f -> Bool
autSub = case autsSub of AutBiReduce cyc rec -> autBiReduce cyc rec

data AutMerge f = forall c . (Ord c) => AutMerge
  { autMerge_ictx :: forall x y . f x -> f y -> c
  , autMerge_imp  :: forall x z m . (Applicative m)
                  => (c -> x -> m z)
                  -> c -> f x -> m (f z)
  , autMerge_rec  :: forall x y z m . (Applicative m)
                  => (c -> x          -> m z)
                  -> (c ->        y   -> m z)
                  -> (c -> x          -> m z)
                  -> (c ->        y   -> m z)
                  -> (c -> x   -> y   -> m z)
                  ->  c -> f x -> f y -> m (f z)
  }

class (T.Traversable f, PE.Ord1 f) => AutStructInter (f :: * -> *) where
  autsInter :: AutMerge f

autInter :: (AutStructInter f, AutomataReprBin a a') => a f -> a' f -> a f
autInter = case autsInter of AutMerge c i r -> autGenMerge c i r

class (T.Traversable f, PE.Ord1 f) => AutStructUnion (f :: * -> *) where
  autsUnion :: AutMerge f

autUnion :: (AutStructUnion f, AutomataReprBin a a') => a f -> a' f -> a f
autUnion = case autsUnion of AutMerge c i r -> autGenMerge c i r

------------------------------------------------------------------------}}}
