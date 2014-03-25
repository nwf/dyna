---------------------------------------------------------------------------
-- | Self-contained, recursive automata, parametric in ply functor.
--
-- Automata parameterised by functor F can be expected to behave as maps
-- from labels to F-structure over labels.  That is, they are labeled
-- descriptions of fixed points of F.  This interface hides the actual
-- labeling strategy in use inside an automaton from the user of the
-- representation.
--
-- Note that particular automata implementations (instances) may have an API
-- that goes (well) beyond what's available here.  In particular, it is
-- expected that non-trivial construction is beyond the scope of this common
-- API.
--
-- XXX Is there a natural extension that allows for epsilon transitions in
-- the definitions?  The typical example of where this comes up is in a
-- generic "disjunction" branch of @f@.  Defining disjunction-on-disjunction
-- or disjunction-on-base cases go through in the current formulation, but
-- disjunction-on-recursion does not.  Is the answer to eliminate the NonRec
-- requirement on lop-sided callbacks?  In fact, maybe we could eliminate
-- those parameters altogether, making the type of the 'autBiReduce'
-- callback
--   @(x -> m r) -> (y -> m r) -> (x -> y -> m r) -> f x -> f y -> m (f r)@
-- Pushing that through would complicate (but maybe not badly) the handling
-- of things like uniqueness in Dyna/Analysis/Mode/Inst.hs:/^iLeqGLB_ ;
-- maybe it should be using @c@ for that anyway.
--
-- XXX How do we generalize this to multi-ply automata?

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.XXX.Automata.ReprClass where

import           Control.Applicative (Applicative)
import qualified Data.Traversable                  as T
import qualified Prelude.Extras                    as PE
import qualified Text.PrettyPrint.Free             as PP

------------------------------------------------------------------------}}}
-- Unary class definitions                                              {{{

-- | The class of a representations of an automata.  The functions here are
-- not the user-friendly operations like @intersect@ and @union@ but are
-- rather designed to provide a reasonably abstract view of the internals of
-- any backing store for automata.
class AutomataRepr (a :: (* -> *) -> *) where
  -- Construction

  -- | An inverse (up to isomorphism) to 'autExpose'.
  --
  -- Note that this may not be efficient, so it should be used only as a
  -- last resort.
  autHide    :: (PE.Ord1 f, T.Traversable f) => f (a f) -> a f

  -- | This is most frequently used as a base case in a lopsided
  -- inductive step, in which one side is known to be acyclic, but the other
  -- may be cyclic.  When we have reached the leaves of the former,
  -- 'autShallow' will yield a trivial automata which may then be passed to
  -- a binary automata function (e.g. 'autCmp') which will deal with cycles.
  --
  -- Parametricity ensures that this function has no access to any but the
  -- top ply of its argument.
  --
  -- A default method is provided in terms of 'autHide' but that may not be
  -- efficient.
  autShallow :: (PE.Ord1 f, T.Traversable f) => f x -> Maybe (a f)

  autShallow f = do
    f' <- T.mapM (const Nothing) f
    return $ autHide f'
  {-# INLINABLE autShallow #-}

  -- Destruction

  -- | Reveal the topmost ply of this automata.
  --
  -- Note that recursive use of this function may well diverge!
  autExpose :: (PE.Ord1 f, Functor f) => a f -> f (a f)

  -- Unary traversals
  autMap :: (Functor f) => (forall x . f x -> f x) -> a f -> a f

  -- | Non-indexed reduction.  Given a cycle-breaking result and an
  -- algebraic interpretation of a single state of the automaton, produce a
  -- result.
  --
  -- Note that cycles are only evaluated once: while a given spine is
  -- active, all observations of that spine will yield the cycle-breaking
  -- value; once a node is no longer on the active spine, observers will
  -- observe its result.  Results of this function are only well-defined if
  -- the callbacks are invariant to node visit order subject to this
  -- observation constraint.
  --
  -- XXX Should we augment this with context?
  autReduce :: forall f r .
               (T.Traversable f)
            => r
            -> (f r -> r)
            -> a f -> r

------------------------------------------------------------------------}}}
-- Binary class definitions                                             {{{

class AutomataReprBin (al :: (* -> *) -> *) (ar :: (* -> *) -> *) where

  -- | Binary reducer; see 'autReduce' for overview.
  --
  -- The HOF here is given three callbacks, for both "in-phase" and
  -- "out-of-phase" work; the former allows simultaneous descent into two
  -- states, while the others allow the merge to remain at its current
  -- position on one side while descending the other.
  --
  -- Unlike many other operators in this library, this one exposes a
  -- 'Monad'ic behavior to the callback, for the recognition that the @r@
  -- value may induce short-circuiting.
  --
  -- XXX Should we augment this with context?
  autBiReduce :: forall f r .
                 (PE.Ord1 f, T.Traversable f)
              => r
              -> (forall x y m .
                     (Monad m)
                  => (x          -> m r)
                  -> (       y   -> m r)
                  -> (x   -> y   -> m r)
                  ->  f x -> f y -> m r)
              -> al f -> ar f -> r

  -- | Total synchronous binary merge; @c@ records the type of top-down
  -- context information needed for what is otherwise a bottom-up operation.
  -- Since multiple paths may reach pairs of states, @c@ is used internally
  -- as part of the index -- that is, a given pair of states may be
  -- given to the callback repeatedly at differing @c@ values.
  --
  -- The core callback is given functions for importing structure from
  -- either the left or the right automata, for descending either while
  -- holding still in the other, and simultaneously descending into both.
  autGenMerge :: forall c f .
              (Ord c, PE.Ord1 f, T.Traversable f)
           => (forall x y . f x -> f y -> c)   -- top-level context function
           -> (forall x z m .
                  (Applicative m)
               => (c -> x   -> m z)
               ->  c -> f x -> m (f z))        -- single-sided import function
           -> (forall x y z m .
                  (Applicative m)
               => (c -> x          -> m z)	 -- single-sided import callbacks
               -> (c ->        y   -> m z)
               -> (c -> x          -> m z)   -- lop-sided descent callbacks
               -> (c ->        y   -> m z)
               -> (c -> x   -> y   -> m z)   -- simultaneous descent callback
               ->  c -> f x -> f y -> m (f z)) -- ply-by-ply merge operation
           -> al f -> ar f -> al f

  -- Default instance in terms of autGenPMerge; ick etc.
  autGenMerge cf i m r l =
    case autGenPMerge cf
                      (\a b c -> fmap Right (i a b c))
                      (\a b c d e f g h -> fmap Right (m a b c d e f g h))
                      r l of
      Left _ -> error "Impossible: autGenMerge default Left"
      Right a -> a
  {-# INLINABLE autGenMerge #-}

  -- | Total synchronous binary merge restricted to the \"intersection\"
  -- case, which does not avail itself of the \"import\" functionality.
  autMerge :: forall c f .
              (Ord c, PE.Ord1 f, T.Traversable f)
           => (forall x y . f x -> f y -> c)   -- top-level context function
           -> (forall x y z m .
                  (Applicative m)
               => (c -> x          -> m z)   -- lop-sided descent callbacks
               -> (c ->        y   -> m z)
               -> (c -> x   -> y   -> m z)   -- simultaneous descent callback
               ->  c -> f x -> f y -> m (f z)) -- ply-by-ply merge operation
           -> al f -> ar f -> al f

  autMerge = \ic cb -> autGenMerge ic (error "autMerge impossible cb")
                                      (\_ _ -> cb)
  {-# INLINABLE autMerge #-}

  -- | Partial synchronous binary merge.
  --
  -- Note that the callbacks are total (applicative) functions: failure is
  -- handled internally to @m@ and may short-circuit.
  autGenPMerge :: forall c e f .
               (Ord c, PE.Ord1 f, T.Traversable f)
            => (forall x y . f x -> f y -> c)
            -> (forall x z m .
                   (Applicative m)
                => (c -> x   -> m z)
                ->  c -> f x -> m (Either e (f z)))
            -> (forall x y z m .
                   (Applicative m)
                => (c -> x          -> m z)
                -> (c ->        y   -> m z)
                -> (c -> x          -> m z)
                -> (c ->        y   -> m z)
                -> (c -> x   -> y   -> m z)
                ->  c -> f x -> f y -> m (Either e (f z)))
            -> al f -> ar f -> Either e (al f)

  -- | Partial synchronous binary merge restricted to the \"intersection\"
  -- case.
  autPMerge :: forall c e f .
               (Ord c, PE.Ord1 f, T.Traversable f)
            => (forall x y . f x -> f y -> c)
            -> (forall x y z m .
                   (Applicative m)
                => (c -> x          -> m z)
                -> (c ->        y   -> m z)
                -> (c -> x   -> y   -> m z)
                ->  c -> f x -> f y -> m (Either e (f z)))
            -> al f -> ar f -> Either e (al f)

  autPMerge = \ic cb -> autGenPMerge ic (error "autPMerge impossible cb")
                                        (\_ _ -> cb)
  {-# INLINABLE autPMerge #-}

------------------------------------------------------------------------}}}
-- Indexed reducer class                                                {{{

-- XXX These methods are probably unwise to offer; ReduceIx existed only for
-- printout, which we now handle differently.  MapIx is unsafe and unused.
class AutomataIxRed a where
  -- | Per-state rewriting function with index visibility.
  --
  -- Callback will be called at most once for each state of the automata,
  -- and at least once for all reachable states, but there may be equivalent
  -- states within as well (but see 'autMinimize').
  --
  -- In this version, the callback has visibility into the states reachable
  -- from the current position within the automata, but no cycle-breaking is
  -- performed by the machinery, so careless use of this ability may
  -- diverge; an 'Ord' instance is asserted for the recursion sites to help
  -- mitigate this situation.
  autMapIx :: forall f . (Functor f)
           => (forall x . (Ord x) => (x -> f x) -> f x -> f x)
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
  --
  autReduceIx :: forall f r x .
                 (T.Traversable f, Enum x, Ord x)
              => (x -> r)
              -> (x -> f r -> r)
              -> a f -> r

------------------------------------------------------------------------}}}
-- Renderable class definition                                          {{{

class AutomataRender a where

  -- | Given a ply-by-ply rendering function, render an automaton.  The
  -- callback should not inspect or manipulate the @Doc e@ in a ply in order
  -- to be a faithful printout of the automaton.
  autRender :: forall e f . (T.Traversable f)
            => (f (PP.Doc e) -> PP.Doc e)
            -> a f -> PP.Doc e

------------------------------------------------------------------------}}}
-- Minimization class definition                                        {{{

class AutomataMinimize a where

  -- | Automata minimization.
  --
  -- XXX PE.Ord1 f, not Ord (f Int)
  autMinimize :: (T.Traversable f, Ord (f Int)) => a f -> a f

------------------------------------------------------------------------}}}

