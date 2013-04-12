---------------------------------------------------------------------------
-- | Execution-oriented aspects of functions we might actually want to
-- call during mode analysis.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.Functions {-(
  -- * Named inst functions
  -- nWellFormed, nGround, nLeq, nLeqGLB, nSub, nSubGLB, nSubLUB,
  -- * Unification
  unifyVV, unifyUnaliasedNV,
  -- * Matching,
  subVN,
  -- * Modes
  doCall, mWellFormed
)-} where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.RWS
-- import           Data.Functor.Identity
import qualified Data.Map                          as M
import qualified Data.Maybe                        as MA
import qualified Data.Set                          as S
import qualified Data.Traversable                  as T
import           Dyna.Analysis.Mode.Execution.Context
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Dyna.XXX.MonadContext
import           Dyna.XXX.MonadUtils
import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Unification                                                          {{{

-- | Constraints common to all unification functions
type UnifCtxC  m f n = (Ord f, Show f,
                       Monad m, MonadError UnifFail m,
                       n ~ NIX f)

-- | Constraints for unification on keyed insts
type UnifCtxKC m f n k = (MCVT m k ~ ENKRI f n k, MCM m k)

-- | Name-on-Name unification, which computes a new name for the result.
--   We assume that the sources will be updated by the caller, if
--   applicable (e.g. 'unifyNK').
unifyNN :: UnifCtxC m f n
        => Bool -> n -> n -> m n
unifyNN l a b =
  either throwError return $ (if l then nLeqGLBRL else nLeqGLBRD) a b

unifyVV :: (UnifCtxC m f n, UnifCtxKC m f n k,
            MCVT m VV ~ VR f n k, MCA m VV)
        => Bool -> VV -> VV -> m VV
unifyVV l vl vr = calias (unifyXX l) vl vr

-- |
--
-- Note that the caller is required to do whatever is necessary to ensure
-- that the resulting 'VR' is interpreted in an aliased context going
-- forward.  That is, we expect to be called by 'calias'.
unifyXX :: (UnifCtxC m f n, UnifCtxKC m f n k)
        => Bool -> VR f n k -> VR f n k -> m (VR f n k)
unifyXX l (VRName   na) (VRName   nb) = liftM VRName $ unifyNN l na nb
unifyXX l (VRName   na) (VRKey    kb) = liftM VRKey  $ unifyNK l na kb
unifyXX l (VRKey    ka) (VRName   nb) = liftM VRKey  $ unifyNK l nb ka


{-
unifyXX l (VRName   na) (VRStruct ub) = do
                                         kb <- aliasY ub
                                         unifyNK l na kb
unifyXX l (VRKey    ka) (VRName   nb) = unifyNK l nb ka
unifyXX l (VRKey    ka) (VRKey    kb) = unifyKK l ka kb
unifyXX l (VRKey    ka) (VRStruct ub) = do
                                         kb <- aliasY ub
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRName   nb) = do
                                         ka <- aliasY ua
                                         unifyNK l nb ka
unifyXX l (VRStruct ua) (VRKey    kb) = do
                                         ka <- aliasY ua
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRStruct ub) = do
                                         ka <- aliasY ua
                                         kb <- aliasY ub
                                         unifyKK l ka kb
-}

-- | Name-on-Key unification.  This updates the key's bindings and leaves
--   the name alone (we assume that the source of the name will be updated
--   by the caller, if applicable)
--
-- This function returns the key given as a convenient shorthand.
unifyNK l n k = cmerge (unifyEE l UUnique) k (Left n) >> return k

-- | Unify two already-aliased bits of structure, returning an inst key
-- arbitrarily.  Any additional aliases encountered will, of course,
-- also be updated as a result.
unifyKK _ a b | a == b = return a
unifyKK l a b          = calias (unifyEE l UUnique) a b

-- | The guts of key-on-key unification; this expects to be called in a
-- context where the keys which produced the input will be suitably
-- rewritten (i.e. by 'calias' or 'cmerge')
unifyEE :: forall f k m n .
           (UnifCtxC m f n, UnifCtxKC m f n k)
        => Bool
        -> Uniq
        -> ENKRI f n k
        -> ENKRI f n k
        -> m (ENKRI f n k)
unifyEE l u (Left  na) (Left  nb) = liftM (Left . nUpUniq u) $ unifyNN l na nb
unifyEE l u (Right ia) (Right ib) = 
   either throwError (return . Right) =<<
   (if l then iLeqGLBRL_ else iLeqGLBRD_)
     (reUniqJ) (reUniqJ)
     (undefined)
     (undefined)
     (undefined)
     UUnique ia ib

lsuQJ l u q (Left n) = unifyEE l u (Right q) (nExpose n)

stepJ :: (n ~ NIX f, Monad m, MCVT m k ~ ENKRI f n k, MCR m k)
      => Either n k -> m (ENKRI f n k)
stepJ (Left  n) = return $ Left n
stepJ (Right k) = either (return . Left) (return . Right) =<< clookup k

reUniqJ :: (Ord f, n ~ NIX f, Monad m, MCVT m k ~ ENKRI f n k, MCM m k)
        => Uniq -> Either n k -> m (Either n k)
reUniqJ u  (Left n)  = return (Left (nUpUniq u n))
reUniqJ u0 (Right k) = cmerge mf k u0 >> return (Right k)
 where
  mf (Left  n) u = return (Left (nUpUniq u n))
  mf (Right i) u = liftM Right $ 
                    T.sequence $ over inst_rec (fmap (fmap (reUniqJ u)))
                               $ over inst_uniq (max u) i

{-
{-
unifyNE :: forall f k m n .
           ( Eq k, Ord f, Ord n, Show f, n ~ NIX f
           , Monad m, MCVT m k ~ ENKRI f n k, MCA m k, MCM m k )
        => Bool -> n -> ENKRI f n k -> 
-}


unifyNK :: forall f k m n .
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
        => Bool
        -> n
        -> k
        -> m k
unifyNK l n k = cmerge (unifyEE l) k (Left n) >> return k

unifyEE :: forall f k m n .
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
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
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
        => Bool
        -> KRI f n k
        -> KRI f n k
        -> m (KRI f n k)
unifyQQ l qa qb = do
  when (not l) $ semidet_clobbered_unify qa qb >>= flip when (fail "UQQ QQ SCU")
  iLeqGLB_ return return (unifyJJ l) qa qb

unifyJJ :: forall f k m n .
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
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
unifyKK :: (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k)
        => Bool
        -> k
        -> k
        -> m k
unifyKK _ a b | a == b = return a
unifyKK l a b          = XT.traceShow ("UKK",a,b) $ calias (unifyEE l) a b

-- | The core of unifyVV, this function operates on two user variable
--   bindings.  When it encounters an unaliased reference it will promote
--   it to aliased and then continue unification, deleting the unaliased
--   inputs.
unifyXX :: forall f k m n .
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k)
        => Bool
        -> VR f n k
        -> VR f n k
        -> m k
unifyXX l (VRName   na) (VRName   nb) = unifyNN l na nb
                                         >>= cnew . const . return . Left
unifyXX l (VRName   na) (VRKey    kb) = unifyNK l na kb
unifyXX l (VRName   na) (VRStruct ub) = do
                                         kb <- aliasY ub
                                         unifyNK l na kb
unifyXX l (VRKey    ka) (VRName   nb) = unifyNK l nb ka
unifyXX l (VRKey    ka) (VRKey    kb) = unifyKK l ka kb
unifyXX l (VRKey    ka) (VRStruct ub) = do
                                         kb <- aliasY ub
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRName   nb) = do
                                         ka <- aliasY ua
                                         unifyNK l nb ka
unifyXX l (VRStruct ua) (VRKey    kb) = do
                                         ka <- aliasY ua
                                         unifyKK l ka kb
unifyXX l (VRStruct ua) (VRStruct ub) = do
                                         ka <- aliasY ua
                                         kb <- aliasY ub
                                         unifyKK l ka kb

-- | Variable-on-variable unification.  Ah, finally.
--
-- Based on figure 5.7, p 104.
--
-- XXX We probably do not handle free-free unification correctly, in light
-- of §5.4.1.  For the moment, I am skipping this.
unifyVV :: forall f k m n v .
           (Eq k, Ord f, Show k, n ~ NIX f,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k,
            MCVT m v ~ VR f n k, MCA m v)
        => (v -> Bool)
        -> v
        -> v
        -> m v
unifyVV lf va vb =
  calias (\a' b' -> liftM VRKey $ unifyXX (lf va || lf vb) a' b') va vb


-- | Name-on-Variable unification.  This should not be called on names
-- looked up from the context, as that would omit important updates to the
-- context for alias tracking; instead, only call this on ``freestanding''
-- named insts which are known for other reasons, such as part of a
-- predicate mode.
--
-- This function returns the variable given as a convenient shorthand.
--
-- Based on figure 5.7, p 104.
unifyUnaliasedNV :: forall f k m n v .
                    (Eq k, Ord f, Show k, n ~ NIX f,
                     MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
                     MCVT m v ~ VR f n k, MCR m v, MCW m v)
                 => Bool
                 -> n
                 -> v
                 -> m v
unifyUnaliasedNV l n0 v0 = do
  x0 <- clookup v0
  xu <- unifyNX n0 x0
  cassign v0 xu
  return v0
 where
  unifyNX :: n -> VR f n k -> m (VR f n k)
  unifyNX na (VRName   nb) = liftM VRName   $ unifyNN l na nb
  unifyNX na (VRKey    kb) = liftM VRKey    $ unifyNK l na kb
  unifyNX na (VRStruct ub) = liftM VRStruct $ unifyNY   na ub

  unifyNY :: n -> InstF f (VR f n k) -> m (InstF f (VR f n k))
  unifyNY na ub = do
    ia <- clookup na
    when (not l) $ semidet_clobbered_unify ia ub >>= flip when (fail "UNU SCU")
    iLeqGLB_ (return . VRName) return unifyNX ia ub
-}

------------------------------------------------------------------------}}}
-- Matching                                                             {{{

{-
subNN :: (Ord f, n ~ NI f, Monad m)
      => n -> n -> m Bool
subNN a b = XT.traceShow ("SNN",a,b) $ return $ nSub a b

iCompare :: (Ord f, n ~ NI f)
         => (forall m' .
                (Monad m')
             => (n -> InstF f n -> m' Bool)
             -> (n -> n -> m' Bool)
             -> InstF f n -> InstF f n -> m' Bool)
         -> InstF f n -> InstF f n -> Bool
iCompare cmp i0 j0 = tieKnotCompare cmp (\qa qb -> cmp qa qb i0 j0)

subNI :: forall f n m .
         (Ord f, Show f, n ~ NI f, Monad m)
      => n -> InstF f n -> m Bool
subNI n i = XT.traceShow ("SNI",n,i) $ do
  ni <- clookup n
  return $ iCompare iSub_ ni i

subJN :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => Either n k -> n -> m Bool
subJN j = either subNN subKN j

subJI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => Either n k -> InstF f n -> m Bool
subJI j = either subNI subKI j

subQI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => KRI f n k -> InstF f n -> m Bool
subQI q i = XT.traceShow ("SQI",q,i) $ iSub_ subJI subJN q i

subQN :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => KRI f n k -> n -> m Bool
subQN q n = XT.traceShow ("SQN",q,n) $ do
  ni <- clookup n
  subQI q ni

subKN :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => k -> n -> m Bool
subKN k n = XT.traceShow ("SKN",k,n) $ do
  kq <- clookup k
  (either subNN subQN kq) n

subKI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => k -> InstF f n -> m Bool
subKI k i = XT.traceShow ("SKI",k,i) $ do
  kq <- clookup k
  (either subNI subQI kq) i

subYI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => InstF f (VR f n k) -> InstF f n -> m Bool
subYI y i = XT.traceShow ("SUI",y,i) $ do
  iSub_ subXI subXN y i

subXI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => VR f n k -> InstF f n -> m Bool
subXI x i = XT.traceShow ("SXI",x,i) $ do
  case x of
    VRName   xn -> subNI xn i
    VRKey    xk -> subKI xk i
    VRStruct xy -> subYI xy i

subXN :: forall f k m n .
         (Ord n, Ord f, Show n, Show f, Show k,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => VR f n k -> n -> m Bool
subXN x n = XT.traceShow ("SXN",x,n) $ do
  case x of
    VRName   xn -> subNN xn n
    VRKey    xk -> subKN xk n
    VRStruct xu -> subUN xu n

subUN :: forall f k m n .
         (Ord n, Ord f, Show n, Show f, Show k,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => InstF f (VR f n k) -> n -> m Bool
subUN u n = XT.traceShow ("SUN",u,n) $ do
  ni <- clookup n
  iSub_ subXI subXN u ni

subVN :: forall f k m n v .
         (Ord n, Ord f, Show n, Show f, Show k, Show v,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k,
          MCVT m v ~ VR f n k, MCR m v)
      => v
      -> n
      -> m Bool
subVN v n = XT.traceShow ("SVN",v,n) $ do
  vx <- clookup v
  case vx of
    VRName   un -> subNN un n
    VRKey    uk -> subKN uk n
    VRStruct ui -> subUN ui n

-- | Enact a particular call.
--
-- Notice that if this fails the unification variables have been updated and
-- so should be discarded.
--
-- XXX This doesn't do mode polymorphism, but it at least seems to be
-- vaguely right at the moment.
--
-- XXX I am confused as to why this is "do all the subs then do all the
-- unifies" but that seems to be how the thesis rule is written unless I
-- misread.
--
-- Based on Figure 5.7, p104
doCall :: forall f k m n v .
          (Ord n, Ord f, Eq k,
           Show n, Show f, Show k, Show v,
           MonadPlus m, MonadFix m,
           MCVT m n ~ InstF f n, MCR m n, MCN m n,
           MCVT m k ~ ENKRI f n k, MCA m k, MCM m k, MCR m k,
           MCVT m v ~ VR f n k, MCR m v, MCW m v)
        => (v -> Bool) -- Liveness predicate
        -> v -> [v]    -- Call with these arguments
        -> QMode n     -- Against this pattern
        -> m Bool
doCall l r0 as0 (QMode cs0 (rmi,rmo)) = go (r0:as0) (rmi:map fst cs0)
 where
  go []     []     = goUnify as0 (rmo:map snd cs0)
  go (a:as) (c:cs) = andM (subVN a c) (go as cs)
  go _      _      = return False

  goUnify []     []     = return True
  goUnify (a:as) (c:cs) = unifyUnaliasedNV (l a) c a >> goUnify as cs
  goUnify _      _      = return False
-}

------------------------------------------------------------------------}}}
-- Merging                                                              {{{

-- XXX Unimplemented


------------------------------------------------------------------------}}}
-- Mode functions                                                       {{{

{-
-- | Check that all names in a mode are indeed well-formed and that all
-- transitions are according to ≼.
mWellFormed :: forall f . (Ord f) => QMode (NI f) -> Bool
mWellFormed (QMode ats vm@(vti,vto)) =
  (all (nWellFormed UClobbered)
       $ vti:vto:concatMap (\(i,o) -> [i,o]) ats)
  &&
  (all (uncurry (flip nLeq)) $ vm:ats)
-}

------------------------------------------------------------------------}}}
