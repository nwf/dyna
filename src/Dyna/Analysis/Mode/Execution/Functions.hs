---------------------------------------------------------------------------
-- | Execution-oriented aspects of functions we might actually want to
-- call during mode analysis.
--

-- Header material                                                      {{{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.Functions(
  -- * Named inst functions
  nWellFormed, nGround, nLeq, nLeqGLB, nSub, nSubGLB, nSubLUB,
  -- * Unification
  unifyVV,
  -- * Matching
) where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Dyna.Analysis.Mode.Execution.Base
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.XXX.MonadContext
import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Named inst functions                                                 {{{

nWellFormed :: forall f m n .
               (MCVT m n ~ InstF f n, MCR m n, Ord n)
            => Uniq
            -> n
            -> m Bool
nWellFormed u0 i0 = evalStateT (q u0 i0) S.empty
 where
  q u v = do
    already <- gets $ S.member (u,v)
    if already
     then return True
     else do
           modify $ S.insert (u,v)
           i <- lift $ clookup v
           iWellFormed_ q u (i :: InstF f n)

nGround :: forall f m n .
           (MCVT m n ~ InstF f n, MCR m n, Ord n)
        => n -> m Bool
nGround i0 = evalStateT (q i0) S.empty
 where
  q v = do
    already <- gets $ S.member v
    if already
     then return True
     else do
           modify $ S.insert v
           i <- lift $ clookup v
           iGround_ q (i :: InstF f n)

nCompare :: (MCVT m n ~ InstF f n, MCR m n, Ord n, Ord f --,
             -- m' ~ StateT (S.Set (v,InstF f v), S.Set (v,v)) m
            )
         => (forall m' .
                (Monad m')
             => (n -> InstF f n -> m' Bool)
             -> (n -> n -> m' Bool)
             -> InstF f n -> InstF f n -> m' Bool)
         -> n -> n -> m Bool
nCompare cmp i0 j0 = evalStateT (qb i0 j0) (S.empty, S.empty)
 where
  qa v j = do
   already <- gets $ S.member (v,j) . fst
   if already
    then return True
    else do
          modify $ over _1 (S.insert (v,j))
          i <- lift $ clookup v
          cmp qa qb i j
  -- XXX? qb vi vj | vi == vj = return True
  qb vi vj = do
   already <- gets $ S.member (vi,vj) . snd
   if already
    then return True
    else do
          modify $ over _2 (S.insert (vi,vj))
          i <- lift $ clookup vi
          j <- lift $ clookup vj
          cmp qa qb i j



nLeq, nSub :: forall f n m .
              (Ord f, Ord n, MCVT m n ~ InstF f n, MCR m n)
           => n -> n -> m Bool
nLeq = nCompare iLeq_ {- :: (Monad m')
                       => (n -> InstF f n -> m' Bool)
                       -> (n -> n -> m' Bool)
                       -> InstF f n -> InstF f n -> m' Bool) -}

nSub = nCompare iSub_ {- :: (Monad m')
                       => (n -> InstF f n -> m' Bool)
                       -> (n -> n -> m' Bool)
                       -> InstF f n -> InstF f n -> m' Bool) -}

nTotalBin :: forall f m m' t n .
             (Ord n, MonadFix m,
              MCVT m n ~ t, MCR m n, MCN m n,
              m' ~ StateT (M.Map (n, n) n) m, t ~ InstF f n)
          => ((n -> n -> m' n)
              -> t -> t -> m' t)
             -> n -> n -> m n
nTotalBin f i0 j0 = evalStateT (q i0 j0) M.empty
 where
  q ni nj | ni == nj = return ni
  q ni nj = do
    cached <- gets $ M.lookup (ni,nj)
    case cached of
      Just i  -> return i
      Nothing -> do
        (_,nk) <- mfix $ \(~(k,_)) -> do
                    nk <- lift $ cnew k
                    modify $ M.insert (ni,nj) nk
                    i <- lift $ clookup ni
                    j <- lift $ clookup nj
                    k' <- f q i (j :: t)
                    return (k',nk)
        return nk

nLeqGLB, nSubGLB :: (MonadFix m, Ord n, MCVT m n ~ InstF f n, MCR m n, MCN m n, Ord f)
                 => n -> n -> m n
nLeqGLB = nTotalBin (iLeqGLB_ return return)
nSubGLB = nTotalBin (iSubGLB_ return return)

nSubLUB :: forall f n m .
           (MonadFix m, Ord f, Ord n,
            MCVT m n ~ InstF f n, MCR m n, MCN m n, MCF m n, Show n)
        => n -> n -> m (Maybe n)
nSubLUB i0 j0 = evalStateT (runMaybeT (q i0 j0)) M.empty
 where
  q ni nj | ni == nj = return ni
  q ni nj = do
    -- XT.traceShow ("Q ENT",ni,nj) $ return ()
    cache <- gets $ M.lookup (ni,nj)
    case cache of
      Just k  -> return k
      Nothing -> do
        (_,nk) <- mfix $ \(~(k,_)) -> do
                    nk <- lift $ lift $ cnew k
                    modify $ M.insert (ni,nj) nk
                    i <- lift $ lift $ clookup ni
                    j <- lift $ lift $ clookup nj
                    mk <- iSubLUB_ return return q i (j :: InstF f n)
                    maybe mzero (\k' -> return (k',nk)) mk
        return nk

------------------------------------------------------------------------}}}
-- Unification                                                          {{{

-- | This predicate is used to ensure that we reject any attempt at
-- unification which could fail (i.e. is semidet, or, possibly better
-- phrased, must traverse the structure of its argument) and may reference
-- clobbered state.
--
-- In words, a unification can enter its arguments whenever
-- 	1. both inputs are not free variables (a free variable turns
-- 	   unification into assignment; two makes it aliasing)
--  2. either input represents more than one possible term
--
-- The thesis will invoke this function (or rather, its negation) to allow a
-- /dead/ unification to succeed.  Live unifications are probably (yes? XXX?)
-- permitted because it's always possible (if unlikely) that some predicate
-- can run with a clobbered input, and if not, we'll fail at that point.
-- A semidet unification, on the other hand, cannot run with a clobbered
-- input.
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
  && (UMostlyClobbered <= iUniq i || UMostlyClobbered <= iUniq i')

-- | Name-on-Name unification, which computes a new name for the result.
--   We assume that the sources will be updated by the caller, if
--   applicable.
unifyNN :: (Ord n, Ord f, Show n,
            MonadFix m,
            MCVT m n ~ InstF f n, MCR m n, MCN m n)
        => Bool -> n -> n -> m n
unifyNN l a b = XT.traceShow ("UNN",a,b) $ do
  when (not l) $ do
    ia <- clookup a
    ib <- clookup b
    semidet_clobbered_unify ia ib >>= flip when (fail "UNN SCU")
  nLeqGLB a b

-- | Name-on-Key unification.  This updates the key's bindings and leaves
--   the name alone (we assume that the source of the name will be updated
--   by the caller, if applicable)
unifyNK :: forall f k m n .
           (Eq k, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
            MCVT m n ~ InstF f n, MCR m n, MCN m n)
        => Bool
        -> n
        -> k
        -> m k
unifyNK l n k = cmerge (unifyEE l) k (Left n) >> return k

unifyEE :: forall f k m n .
           (Eq k, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
            MCVT m n ~ InstF f n, MCR m n, MCN m n)
        => Bool
        -> ENKRI f n k
        -> ENKRI f n k
        -> m (ENKRI f n k)
unifyEE l (Left  na) (Left  nb) = liftM Left $ unifyNN l na nb
unifyEE l (Left  na) (Right qb) = do
    ia <- clookup na
    when (not l) $ semidet_clobbered_unify ia qb >>= flip when (fail "UEE NQ SCU")
    liftM Right $ iLeqGLB_ (return . Left) return
                           (\n j -> unifyJJ l (Left n) j) ia qb
unifyEE l (Right qa) (Left  nb) = do
    ib <- clookup nb
    when (not l) $ semidet_clobbered_unify qa ib >>= flip when (fail "UEE QN SCU")
    liftM Right $ iLeqGLB_ return (return . Left)
                           (\j n -> unifyJJ l j (Left n)) qa ib
unifyEE l (Right qa) (Right qb) = liftM Right $ unifyQQ l qa qb

unifyQQ :: forall f k m n .
           (Eq k, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
            MCVT m n ~ InstF f n, MCR m n, MCN m n)
        => Bool
        -> KRI f n k
        -> KRI f n k
        -> m (KRI f n k)
unifyQQ l qa qb = do
  when (not l) $ semidet_clobbered_unify qa qb >>= flip when (fail "UQQ QQ SCU")
  iLeqGLB_ return return (unifyJJ l) qa qb

unifyJJ :: forall f k m n .
           (Eq k, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
            MCVT m n ~ InstF f n, MCR m n, MCN m n)
        => Bool
        -> Either n k
        -> Either n k
        -> m (Either n k)
unifyJJ l (Left  na) (Left nb)  = liftM Left  $ unifyNN l na nb
unifyJJ l (Left  na) (Right kb) = liftM Right $ unifyNK l na kb
unifyJJ l (Right ka) (Left  nb) = liftM Right $ unifyNK l nb ka
unifyJJ l (Right ka) (Right kb) = liftM Right $ unifyKK l ka kb

-- | Unify two already-aliased bits of structure, returning an inst key
-- arbitrarily.  Any additional aliases will, of course, also be updated as
-- a result.
unifyKK :: (Eq k, Ord f, Ord n, MonadFix m, Show k, Show n,
            MCVT m n ~ InstF f n, MCR m n, MCN m n,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
        => Bool
        -> k
        -> k
        -> m k
unifyKK _ a b | a == b = return a
unifyKK l a b          = XT.traceShow ("UKK",a,b) $ calias (unifyEE l) a b

-- | Unify two previously unaliased bits of structure into an aliased piece
--   of structure.
--
--   Deletes inputs from unaliased table.
unifyUU :: (Eq u, Eq k, Ord f, Ord n, MonadFix m, Show k, Show n,
            MCVT m n ~ InstF f n, MCR m n, MCN m n,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k,
            MCVT m u ~ InstF f (VR n k u), MCD m u, MCR m u)
        => Bool -> u -> u -> m k
unifyUU _ a b | a == b = cdelete a >>= aliasW
unifyUU l a b          = do
  ka <- cdelete a >>= aliasW
  kb <- cdelete b >>= aliasW
  unifyKK l ka kb

-- | The core of unifyVV, this function operates on two user variable
--   bindings.  When it encounters an unaliased reference it will promote
--   it to aliased and then continue unification, deleting the unaliased
--   inputs.
unifyXX :: forall f k m n u .
           (Eq k, Eq u, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k,
            MCVT m n ~ InstF f n, MCN m n, MCR m n,
            MCVT m u ~ UR f n k u, MCD m u, MCR m u)
        => Bool
        -> VR n k u
        -> VR n k u
        -> m k
unifyXX l (VRName   na) (VRName   nb) = unifyNN l na nb >>= cnew . Left
unifyXX l (VRName   na) (VRKey    kb) = unifyNK l na kb
unifyXX l (VRName   na) (VRStruct ub) = do
                                         kb <- cdelete ub >>= aliasW
                                         unifyNK l na kb
unifyXX l (VRKey    ka) (VRName   nb) = unifyNK l nb ka
unifyXX l (VRKey    ka) (VRKey    kb) = unifyKK l ka kb
unifyXX l (VRKey    ka) (VRStruct ub) = do
                                         kb <- cdelete ub >>= aliasW
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRName   nb) = do
                                         ka <- cdelete ua >>= aliasW
                                         unifyNK l nb ka
unifyXX l (VRStruct ua) (VRKey    kb) = do
                                         ka <- cdelete ua >>= aliasW
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRStruct ub) = unifyUU l ua ub

-- | Variable-on-variable unification.  Ah, finally.
unifyVV :: forall f k m n u v .
           (Eq k, Eq u, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k,
            MCVT m n ~ InstF f n, MCN m n, MCR m n,
            MCVT m u ~ UR f n k u, MCD m u, MCR m u,
            MCVT m v ~ VR n k u, MCA m v)
        => v
        -> v
        -> m v
unifyVV va vb = calias (\a' b' -> liftM VRKey $ unifyXX True {- XXX -} a' b') va vb


------------------------------------------------------------------------}}}
