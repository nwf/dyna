---------------------------------------------------------------------------
-- | Self-contained, mu-recursive inst automata, using the generic Automata
-- framework.

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.NamedInst (
    -- * Datatype definition
    NIX(..),
    -- * Unary functions
    -- ** Well-formedness predicates
    nWellFormedUniq,
    -- ** Inquiries
    nAllNotEmpty, nSomeNotEmpty, nGround,
    -- ** Construction
    nHide, nShallow, nDeep, nFromMap,
    -- ** Destruction
    nExpose, 
    -- ** Rewrites
    nUpUniq, nMinimize,
    -- ** Display
    renderNIX,
    -- * Binary comparators
    nEq, nLeq, nSub,
    -- * Total binary functions
    nLeqGLB, nSubGLB,
    -- * Partial binary functions
    nLeqGLBRD, nLeqGLBRL, -- nSubLUB,
    -- * Mode functions
    mWellFormed,
) where

import           Control.Lens
import           Control.Monad.Identity
import           Control.Monad.State
import qualified Data.Foldable                     as F
import qualified Data.Map                          as M
import qualified Data.Traversable                  as T
import           Dyna.Analysis.Automata.Class
import           Dyna.Analysis.Automata.NamedAut
import           Dyna.Analysis.Automata.Utilities
import           Dyna.Analysis.Mode.Inst
import qualified Dyna.Analysis.Mode.InstPretty     as IP
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Datatype definition                                                  {{{

newtype NIX f = NIX { unNIX :: NA (InstF f) }

-- | Semantic, not structural, equality
instance (Ord f) => Eq (NIX f) where
 n1 == n2 = nEq n1 n2

------------------------------------------------------------------------}}}
-- Pretty-printing                                                      {{{

-- | Render a NIX exactly as it stands (the 'Pretty' instance performs
-- minimization first).
renderNIX :: forall f e . (Show f) => NIX f -> Doc e
renderNIX = autRender (IP.compactly (text.show) id) . unNIX

-- | Pretty-printing minimizes automata first
instance (Show f, Ord f) => Pretty (NIX f) where
  pretty (nMinimize -> n) = renderNIX n

instance (Show f, Ord f) => Show (NIX f) where
  show = show . pretty

------------------------------------------------------------------------}}}
-- Unary predicates                                                     {{{

-- | Check well-formedness of an inst at a given Uniq.  All uniqueness
-- annotations within the inst are required to be larger (i.e. less unique,
-- more restrictive).

-- XXX This formulation does not use iWellFormed_.
-- XXX I'm not sure that it's correct, either.  It has just enough hacks
-- that I am uncomfortable.
nWellFormedUniq :: forall f . Uniq -> NIX f -> Bool
nWellFormedUniq u0 (NIX n) =
  case autReduce (Just UClobbered) go n of
    Nothing -> False
    Just u  -> u0 <= u
 where
  go fmu = let ufu = maybe UClobbered id $ iUniq fmu
               mfu = T.sequence fmu
           in mfu >>= \fu -> if ufu <= minimum (UClobbered : F.toList fu)
                              then Just ufu
                              else Nothing

{-
-- | Check well-formedness of an inst at a given Uniq.  All uniqueness
-- annotations within the inst are required to be larger (i.e. less unique,
-- more restrictive).
nWellFormedUniq :: forall f . (Show f) => Uniq -> NIX f -> Bool
nWellFormedUniq u0 n0@(NIX i0 m) = evalState (iWellFormed_ q u0 i0)
                                             M.empty
 where
  q u a = -- XT.traceShow ("NWFU Q",u,a,ml n0 m a) $
          do
           cached <- gets (M.lookup a)
           case cached of
             Nothing -> rec
                -- If we've been here before, it's OK if we are coming in
                -- more uniquely.  If we are coming in less uniquely (i.e.
                -- with a greater Uniq), then we need to recurse through
                -- this binding again.
             Just u' -> orM1 (u <= u') rec
   where
    rec = do
           id %= M.insert a u
           eml n0 (return . nWellFormedUniq u)
                  (iWellFormed_ q u)
                  m a
-}


-- | Is a named inst ground?
--
-- Note that 'ground' here means 'ground' like the thesis means ground
-- (see def 3.2.17, p52), which includes everything from UUnique to
-- UClobbered.  This may not be what you mean.
nGround :: forall f . NIX f -> Bool
nGround = autReduce True (runIdentity . iGround_ return) . unNIX

nNotEmpty_ :: forall f .
              ([Bool] -> Bool)
           -> NIX f
           -> Bool
nNotEmpty_ disj = autReduce True (runIdentity . visit) . unNIX
 where
  visit IFree     = return True
  visit (IUniv _) = return True
  visit (IAny _)  = return True
  visit (IBound _ m b) = return (disj (b:fmap and (F.toList m)))
{-# INLINABLE nNotEmpty_ #-}

-- | Is there some term not ruled out by this inst?
--
-- This is mostly useful for the test harness, not actual reasoning, at
-- the moment, since we are not sufficiently precise (i.e. we will miss some
-- empty unification results).
--
-- Note in particular for InstF, the only non-accepting states are
-- @IBound _ mempty False@; all the others include some term.
nSomeNotEmpty :: forall f . NIX f -> Bool
nSomeNotEmpty = nNotEmpty_ or
{-# INLINABLE nSomeNotEmpty #-}

-- | Like 'nSomeNotEmpty' but conjunctive across choices -- that is, this
-- requires that all possible branches of an automata are non-empty, rather
-- than 'nSomeNotEmpty', which only checks that there is some input which
-- is accepted by the automata.
nAllNotEmpty :: forall f . NIX f -> Bool
nAllNotEmpty = nNotEmpty_ and
{-# INLINABLE nAllNotEmpty #-}

nUpUniq :: forall f . (Ord f) => Uniq -> NIX f -> NIX f
nUpUniq u0 = NIX . autAtEachState (\_ -> over inst_uniq (max u0)) . unNIX
{-# INLINABLE nUpUniq #-}

-- | Expose the root ply of a 'NIX' as an Inst which recurses as additional
-- 'NIX' elements.
--
-- Note that recursive use of this function may well diverge!
nExpose :: NIX f -> InstF f (NIX f)
nExpose = fmap NIX . autExpose . unNIX
{-# INLINABLE nExpose #-}

nHide :: InstF f (NIX f) -> NIX f
nHide = NIX . autHide . fmap unNIX
{-# INLinABLE nHide #-}

nShallow :: (Show f) => InstF f x -> Maybe (NIX f)
nShallow = fmap NIX . autShallow
{-# INLINABLE nShallow #-}

nDeep :: (Functor m, Monad m, Enum ix, Ord ix)
      => (forall t . (MonadTrans t, Monad (t m))
           => r -> ix -> t m (Either (NIX f) (InstF f (Either ix r))))
      -> r -> m (NIX f)
nDeep f r = liftM NIX $ naUnfold (\r' ix' -> liftM (left' unNIX) $ f r' ix') r
{-# INLINABLE nDeep #-}

nFromMap :: forall f oix.
            Ord oix
         => M.Map oix (InstF f oix) -- ^ recursion table
         -> oix                     -- ^ root
         -> NIX f
nFromMap m r = NIX (naFromMap m r)
{-# INLINABLE nFromMap #-}

------------------------------------------------------------------------}}}
-- Binary predicates                                                    {{{

nEq, nLeq, nSub :: (Ord f) => NIX f -> NIX f -> Bool
nEq (NIX l) (NIX r) = autBiReduce True (\_ _ -> iEq_) l r
nLeq (NIX l) (NIX r) = autBiReduce True (\x _ -> iLeq_ x) l r
nSub (NIX l) (NIX r) = autBiReduce True (\x _ -> iSub_ x) l r

------------------------------------------------------------------------}}}
-- Binary functions                                                     {{{

{-

XXX BITROTTED; NOT YET -- need better understanding of the problem.  The ⊔
function is particularly interesting and it is not yet clear how to define
its recursion in a way that is not painfully special-cased.  At this instant
I have other things that can demand attention.

nSubLUB :: forall f . (Ord f, Show f) => NIX f -> NIX f -> Maybe (NIX f)
nSubLUB = nPBin (\il ir ll lr m -> iSubLUB_ il ir (ll UClobbered) (lr UClobbered) (m UClobbered))
-}

ctxUniq :: InstF f a -> InstF f' a' -> Uniq
ctxUniq x y = u x `max` u y
 where
  u = maybe UUnique id . iUniq

-- XXX This may suggest that we could eliminate the "import" callbacks on
-- iLeqGLB_ and friends now that we're not doing anything special with them.

bendImpl :: forall f i i' i'' m r .
            TyILeqGLB_ f m i i' i'' r
         -> (Uniq -> i -> NonRec (InstF f)  -> m i'')
         -> (Uniq -> NonRec (InstF f) -> i' -> m i'')
         -> (Uniq -> i -> i' -> m i'')
         -> Uniq -> InstF f i -> InstF f i' -> r
bendImpl x lsml lsmr merge = x impl impr lsml' lsmr merge
   where
    lsml' :: Uniq -> NonRec (InstF f) -> i -> m i''
    lsml' u r l = lsml u l r

    impl :: Uniq -> i -> m i''
    impl u l = lsml u l IFree

    impr :: Uniq -> i' -> m i''
    impr u r = lsmr u IFree r

nLeqGLB, nSubGLB :: forall f . (Ord f) => NIX f -> NIX f -> NIX f
nLeqGLB (NIX l) (NIX r) = NIX $ autMerge ctxUniq (bendImpl iLeqGLB_) l r
nSubGLB (NIX l) (NIX r) = NIX $ autMerge (\_ _ -> ()) go l r
 where
  go lsml lsmr merge () = iSubGLB_ (flip (lsml ())) (lsmr ()) (merge ())

nLeqGLBRD, nLeqGLBRL :: (Ord f) => NIX f -> NIX f -> Either UnifFail (NIX f)
nLeqGLBRD (NIX l) (NIX r) = fmap NIX $ autPMerge ctxUniq (bendImpl iLeqGLBRD_) l r
nLeqGLBRL (NIX l) (NIX r) = fmap NIX $ autPMerge ctxUniq (bendImpl iLeqGLBRL_) l r

------------------------------------------------------------------------}}}
-- Mode functions                                                       {{{

-- | Check that all names in a mode are indeed well-formed and that all
-- transitions are according to ≼.
--
-- This lives in Execution.NamedInst because it requires that we be using
-- named insts within the 'QMode'.
--
-- See prose, p35.
mWellFormed :: forall f . (Ord f, Show f) => QMode (NIX f) -> Bool
mWellFormed (QMode ats vm@(vti,vto) _) =
  (all (nWellFormedUniq UUnique)
       $ vti:vto:concatMap (\(i,o) -> [i,o]) ats)
  &&
  (all (uncurry (flip nLeq)) $ vm:ats)

------------------------------------------------------------------------}}}
-- Cleanup and minimization                                             {{{

nMinimize :: (Ord f) => NIX f -> NIX f
nMinimize = NIX . autMinimize . unNIX

------------------------------------------------------------------------}}}
