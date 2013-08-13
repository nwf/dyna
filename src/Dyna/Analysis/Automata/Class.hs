---------------------------------------------------------------------------
-- | Self-contained, recursive automata, parametric in ply functor.
--
-- Note that particular automata implementations (instances) may have an API
-- that goes (well) beyond what's available here.  In particular, it is
-- expected that non-trivial construction is beyond the scope of this common
-- API.

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Automata.Class where

import qualified Data.Traversable                  as T

------------------------------------------------------------------------}}}
-- Utility type definitions                                             {{{

-- | An alias for universal quantification forcing a non-recursive ply of @f@:
-- since there is no defined data of fully polymorphic type, any recursive
-- positions in @f@ must not contain data.
type NonRec f = forall x . f x

------------------------------------------------------------------------}}}
-- Basic class definition                                               {{{

class Automata (a :: (* -> *) -> *) where
  -- Construction

  -- | An inverse (up to isomorphism) to 'autExpose'.
  --
  -- Note that this may not be efficient, so it should be used only as a
  -- last resort.
  autHide    :: (T.Traversable f) => f (a f) -> a f

  -- | This is most frequently used as a base case in a lopsided
  -- inductive step, in which one side is known to be acyclic, but the other
  -- may be cyclic.  When we have reached the leaves of the former,
  -- 'autShallow' will yield a trivial automata which may then be passed to
  -- a binary automata function (e.g. 'autCmp') which will deal with cycles.
  --
  -- Parametricity ensures that this function has no access to any but the
  -- top ply of its argument.
  autShallow :: (T.Traversable f) => f x -> Maybe (a f)

  -- Destruction

  -- | Reveal the topmost ply of this automata.
  --
  -- Note that recursive use of this function may well diverge!
  autExpose :: (Functor f) => a f -> f (a f)

  -- Unary traversals

  -- | Per-state rewriting function.  Callback will be called at most once
  -- for each state of the automata, and at least once for all reachable
  -- states, but there may be equivalent states within as well (but see
  -- 'autMinimize').  The callback has visibility into the states reachable
  -- from the current position within the automata, but no cycle-breaking is
  -- performed by the machinery, so careless use of this ability may
  -- diverge; an 'Ord' instance is asserted for the recursion sites to help
  -- mitigate this situation.
  autAtEachState :: forall f .
                    (forall x . (Ord x) => (x -> f x) -> f x -> f x)
                 -> a f -> a f

  -- | (Indexed) algebraic reduction of an automaton.  Given a
  -- cycle-breaking result (as a function of node identity)
  -- and an algebraic interpretation of a single
  -- state of the automaton (indexed, again, by node identity),
  -- produce a result.
  --
  -- Note that cycles are only evaluated once: while a given spine is
  -- active, all observations of that spine will yield the cycle-breaking
  -- value; once a node is no longer on the active spine, observers will
  -- observe its result.  Results of this function are only well-defined
  -- if the callbacks are invariant to node visit order subject to this
  -- observation constraint.
  --
  -- Note that one should not expect to be able to fully reconstruct the
  -- automata given only the observations made by this function, as the
  -- states may have meaning internal to automata (e.g. interpreted subject
  -- to equality constraints or just as regular structure).
  autReduceIx :: forall f r x .
                 (T.Traversable f, Enum x, Ord x)
              => (x -> r)
              -> (x -> f r -> r)
              -> a f -> r

  -- | Non-indexed reduction.  See 'autReduceIx'.
  --
  -- This could be written in terms of 'autReduceIx', but this may be
  -- more efficient by not having to map state names.
  autReduce :: forall f r .
               (T.Traversable f)
            => r
            -> (f r -> r)
            -> a f -> r

  -- Binary traversals
  --
  -- XXX Possibly these want to be split out to an AutBin class, enabling us
  -- to specify more than just the diagonals in the space of automata?

  -- | Binary reducer; see 'autReduce' for overview.
  --
  -- The HOF here is given three callbacks, for both "in-phase" and
  -- "out-of-phase" work; the former allows simultaneous descent into two
  -- states, while the others allow the merge to remain at a leaf state in
  -- one automata while descending the other.
  autBiReduce :: forall f r .
                 (Ord (f ()))
              => r
              -> (forall x y m .
                     (Monad m)
                  => (x        -> NonRec f -> m r)
                  -> (NonRec f -> y        -> m r)
                  -> (x        -> y        -> m r)
                  ->  f x      -> f y      -> m r)
              -> a f -> a f -> r

  -- | Total synchronous binary merge; @c@ records the type of top-down
  -- context information needed for what is otherwise a bottom-up operation.
  -- Since multiple paths may reach pairs of states, @c@ is used internally
  -- as part of the index -- that is, a given pair of states may be
  -- given to the callback repeatedly at differing @c@ values.
  autMerge :: forall c f .
              (Ord c, Ord (f ()))
           => (forall x y . f x -> f y -> c)
           -> (forall x y z m .
                  (Monad m)
               => (c -> x        -> NonRec f -> m z)
               -> (c -> NonRec f -> y        -> m z)
               -> (c -> x        -> y        -> m z)
               ->  c -> f x      -> f y      -> m (f z))
           -> a f -> a f -> a f


  -- | Partial synchronous binary merge.
  --
  -- Note that the callbacks are total (monadic) functions: failure is
  -- handled internally to @m@ and may short-circuit.
  autPMerge :: forall c e f .
               (Ord c, Ord (f ()))
            => (forall x y . f x -> f y -> c)
            -> (forall x y z m .
                   (Monad m)
                => (c -> x        -> NonRec f -> m z)
                -> (c -> NonRec f -> y        -> m z)
                -> (c -> x        -> y        -> m z)
                ->  c -> f x      -> f y      -> m (Either e (f z)))
            -> a f -> a f -> Either e (a f)

------------------------------------------------------------------------}}}
-- Minimization class definition                                        {{{

class AutMinimize a where

  -- | Automata minimization.
  autMinimize :: (T.Traversable f, Ord (f Int)) => a f -> a f

------------------------------------------------------------------------}}}
