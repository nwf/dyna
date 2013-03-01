---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system
--
-- For this module and all modules under Dyna.Analysis.Mode, we are
-- incredibly indebted to David Overton and the Mercury Prolog system:
-- David Overton. Precise and Expressive Mode Systems for Typed Logic
-- Programming Languages.  University of Melbourne, Department of Computer
-- Science and Software Engineering.  Ph.D. thesis. December, 2003.
-- <http://www.mercury.csse.unimelb.edu.au/information/papers.html#dmo-thesis>
--
-- The sub-modules have been organized in an effort to be as transparently
-- ascriptive to the thesis as possible.  This module is primarily concerned
-- with execution details which are not part of the thesis exposition,
-- primarily by running computable algorithms over the regular trees of the
-- thesis.

-- XXX This file is, at this point, mostly detritus from development and
-- will be cleaned up real soon now.

-- Header material                                                      {{{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode(

  iuWellFormed, iuCompare, iuLeq, iuSub,
  -- * Instantiation Key Maps
  -- InstKeyMap(..), InstKeyVal(..),
  -- * Instantiation contexts
  -- IMLookup(..), IMAssign(..),
  -- * Modes
  -- Mode(..), -- mWellFormed,

  module Dyna.Analysis.Mode.Det,
  module Dyna.Analysis.Mode.Uniq,
  module Dyna.Analysis.Mode.Inst,
  -- module Dyna.Analysis.Mode.InstMap,
  module Dyna.Analysis.Mode.Execution.Base,
  module Dyna.Analysis.Mode.Execution.Functions,
) where

import           Control.Applicative
import           Control.Exception(assert)
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.Foldable            as F
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Data.Traversable         as T
import           Dyna.Analysis.Mode.Execution.Base
import           Dyna.Analysis.Mode.Execution.Functions
import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Inst
-- import           Dyna.Analysis.Mode.InstMap
import           Dyna.Analysis.Mode.Uniq
import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
import           Dyna.XXX.MonadUtils

------------------------------------------------------------------------}}}
-- Instantiation contexts                                               {{{

{-
class (Monad m, Ord v) => IMLookup m f v | m -> f v where
  vlookup :: v -> m (InstF f v)

class (Monad m, Ord v, IMLookup m f v) => IMAssign m f v where
  vassign :: v -> InstF f v -> m ()
-}

{-
-- | Very often we need to recurse through the variable context and stop as
-- soon as we loop.  (Since variables may well be knot-tied to support
-- mu-recursive types, for example.)
--
-- The folder is applicative
ctxRecurse :: (Ord v, Ord a, IMLookup m f v)
           => (a -> InstF f v -> Either [(v,a)] r)   -- ^ Single step
           -> (r -> m r -> m r)                      -- ^ Folder
           -> r                                      -- ^ Zero answer
           -> r                                      -- ^ Cycle answer
           -> a -> InstF f v -> m r
ctxRecurse step fold zr cr = go S.empty
 where
  go stk a i = case step a i of
                 Left is -> foldM (fold_ stk) zr is
                 Right r -> return r

  fold_ stk r (v,a) = if S.member (v,a) stk
                       then fold r (return cr)
                       else fold r $ do
                                      i <- vlookup v
                                      go (S.insert (v,a) stk) a i
-}

------------------------------------------------------------------------}}}
-- Instantiation operations                                             {{{


iuWellFormed :: forall f i m v .
                (MCVT m i ~ (InstF f i), MCR m i, Ord i,
                 MCVT m v ~ (Either i (InstF f (Either v i))), MCR m v, Ord v)
             => Uniq -> v -> m Bool
iuWellFormed u0 v0 = evalStateT (q u0 v0) S.empty
 where
  qe u (Left  v) = q u v
  qe u (Right i) = lift $ nWellFormed u i

  q :: Uniq -> v -> StateT (S.Set v) m Bool
  q u vu = do
    looped <- gets $ S.member vu
    if looped
     then return False
     else do
           modify $ S.insert vu
           i <- lift $ clookup vu
           case i of
             Left vi  -> lift $ nWellFormed u vi
             Right i' -> iWellFormed_ qe u i'


iuCompare :: forall eiv f i ir m tm v .
             (MCVT m i ~ InstF f i, MCR m i, Ord i, Ord f,
              MCVT m v ~ Either i ir, MCR m v, Ord v,
              tm ~ StateT (S.Set (eiv,ir), S.Set (eiv,eiv)) m,
              ir  ~ InstF f eiv,
              eiv ~ Either i v)
         => ( forall m' . (Monad m')
             => (eiv -> ir  -> m' Bool)
             -> (eiv -> eiv -> m' Bool)
             -> ir -> ir -> m' Bool)
         -> v -> v -> m Bool
iuCompare cmp i0 j0 = evalStateT (qb (Right i0) (Right j0)) (S.empty, S.empty)
 where
  rq = cmp qa qb

  ll :: eiv -> tm ir
  ll = either (liftM (fmap Left) . lift . clookup) lq
   where
    lq x = do
      x' <- lift $ clookup x
      either (liftM (fmap Left) . lift . clookup) return x'

  qa (v :: eiv) j = do
   already <- gets $ S.member (v,j) . fst
   if already
    then return True
    else do
          modify $ over _1 (S.insert (v,j))
          i <- ll v
          rq i j
  -- XXX? qb vi vj | vi == vj = return True
  qb (vi :: eiv) (vj :: eiv) = do
   already <- gets $ S.member (vi,vj) . snd
   if already
    then return True
    else do
          modify $ over _2 (S.insert (vi,vj))
          i <- ll vi
          j <- ll vj
          rq i j

iuLeq, iuSub :: forall f i v m .
                (Ord i, Ord f, Ord v, Monad m,
                 MCVT m i ~ InstF f i, MCR m i,
                 MCVT m v ~ Either i (InstF f (Either i v)), MCR m v)
             => v -> v -> m Bool
iuLeq = iuCompare iLeq_
iuSub = iuCompare iSub_



------------------------------------------------------------------------}}}
-- Instantiation Maps                                                   {{{

{-
chkial m = goV S.empty S.empty
 where
  goV vis stk v = case M.lookup v m of
                Just i -> go vis stk i
                Nothing -> Left v

  goI vis stk (IAlias v) =
    case () of
      _ | v `S.member` stk -> Left v               -- ERR: cycle
      _ | v `S.member` vis -> Right vis            -- OK: already cleared
      _ -> goV (S.insert v vis) (S.insert v stk) -- WORK: chase cycle
  goI vis _   _ = Right vis                      -- OK: Productive step

-- | Test the well-formedness of an 'InstMap' and return a witness variable
-- that is problematic, if any.
imWellFormed :: InstMap f v -> Maybe v
imWellFormed IMNotReached = Nothing
imWellFormed (IM m) = foldr checker S.empty (M.keys m)


go S.empty S.empty (M.keys m)
 where
  go _   _  []     = Nothing
  go vis as (v:vs) =
   if v `S.elem` vis
    then go vis S.empty vs        -- Been here and it's OK
    else case M.lookup v m of
           Nothing -> Just v      -- Whoop, that's bad.
           Just i -> check vs vis as i
-}

{-
refineIM_ v i i' m s =
  case i `iLeqGLB_` i' of
    Nothing -> IMNotReached (M.delete v m) (S.insert v s)
    -- XXX Just (INotReached _) -> IMNotReached (M.delete v m) (S.insert v s)
    Just iglb -> IM (M.insert v iglb m)

refineIM :: (Ord v, Eq f) => v -> InstF f i -> InstMap f v i -> InstMap f v i
refineIM v i (IM m) =
  let i0 = maybe IFree id $ M.lookup v m in
  assert (i `iLeq_` i0) $ refineIM_ v i i0 m S.empty
refineIM v _ im@(IMNotReached _ s) | v `S.member` s = im
refineIM v i (IMNotReached m s) =
  let i0 = maybe IFree id $ M.lookup v m in
  assert (i `iLeq_` i0) $ refineIM_ v i i0 m s
-}


{-
instance (Monad m, Ord v) => IMLookup (StateT (InstMap f v v) m) f v where
  vlookup v = get >>= return . imLookup v
-}


------------------------------------------------------------------------}}}
-- Instantiation Key Maps                                               {{{

{-
-- | An instantiation key map is the representation of the prolog abstract
-- machine's binding patterns.  That is, it maps every prolog variable @u@
-- to another variable or an Inst, which in turn recurses to contain either
-- prolog variables or defined (possibly recursive)
newtype InstKeyMap f u v = IKM { unIKM :: M.Map u (InstKeyVal f u v) }

data InstKeyVal f u v = IKVUser u
                      | IKVName v
                      | IKVInst (InstF f (Either u v))

class (Monad m, Ord u) => IKMLookup m f u v | m -> f u v where
  ulookup :: u -> m (InstKeyVal f u v)

class (Monad m, Ord u, IKMLookup m f u v) => IKMAssign m f u v where
  uassign :: u -> InstKeyVal f u v -> m ()

ikmSemiprune :: (Ord u, MonadState (InstKeyMap f u v) m) => u -> m u
ikmSemiprune u = do
    m <- gets unIKM
    let (u', m') = mapSemiprune q IKVUser u m
    put (IKM m')
    return u'
 where q (IKVUser u') = Just u'
       q _            = Nothing

instance (Monad m, Ord u) => IKMLookup (StateT (InstKeyMap f u v) m) f u v where
  ulookup u = do
    u' <- ikmSemiprune u
    m' <- get
    case M.lookup u' (unIKM m') of
      Just (IKVUser u'') | u' == u'' -> do
                               put $ IKM $ M.insert u' (IKVInst IFree) (unIKM m')
                               return (IKVUser u')
      Just (IKVUser _) -> error "ikmSemiprune bug"
      Just i  -> return i
      Nothing -> error "ulookup XXX" -- XXX

{-
instance (Monad m, Ord u) => IKMAssign (StateT (InstKeyMap f u v) m) f u v where
  uassign u ikv = modify (IKM . M.insert u ikv . unIKM)
-}

{-
abstract_unify :: InstKeyMap f u v -> u -> u -> Maybe (InstKeyMap f u v)
abstract_unify ikm u1 u2 =
  u1'  <- ikmSemiprune u1 ikm
  u2'  <- ikmSemiprune u2 ikm
  abstract_unify_core ikm'' u1' u2'

abstract_unify_core :: InstKeyMap f u v -> InstKeyVal f u v -> InstKeyVal f u v -> Maybe (InstKeyMap f u v)
abstract_unify_core = undefined
-}

-}

------------------------------------------------------------------------}}}
-- Abstract Interpretation                                              {{{




------------------------------------------------------------------------}}}
-- Mode                                                                 {{{

-- XXX While this type works fine for the thesis, for us it's a little bit
-- of bad news: we need to have both of these maps in scope as we go about
-- reasoning.  That means we're going to need to freshen these into our
-- current global store and do operations there.  It may not be so bad, but
-- this also kind of sucks.
--
-- It's also another argument in favor of some later attempt to have the
-- scope maps canonicalize themselves as we go.

-- | Mode
--
-- Definition 3.1.10, p34
-- data Mode f v i = Mode { m_ini :: InstMap f v i
--                        , m_fin :: InstMap f v i}

{-
-- | Well-formedness condition on modes.  Definition 3.1.10, p35
mWellFormedV :: (MCR m v (InstF f v), Ord f, Ord v) => Mode f v v -> m Bool
mWellFormedV (Mode {m_init = mi, m_fin = mf}) =
 let kmi = imKeys mi
 in  andM1 (kmi == (imKeys mf)) $
       setForallM (\v -> imLookup v mf `iLeq` imLookup v mi) kmi
-}
------------------------------------------------------------------------}}}
