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
  unifyVV, unifyNV,
  -- * Matching,
  subVN,
  -- * Calls
  doCall,
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
import           Dyna.XXX.MonadUtils
import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Named inst functions                                                 {{{

-- These functions all use StateT transformers to handle cyclic reasoning.
--
-- XXX It is possible that we will be hitting some of these tests often and
-- should do some form of caching in our contexts.

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

tieKnotCompare :: (MCVT m n ~ InstF f n, MCR m n, Ord n, Ord f)
               => (forall m' .
                      (Monad m')
                   => (n -> InstF f n -> m' Bool)
                   -> (n -> n -> m' Bool)
                   -> InstF f n -> InstF f n -> m' Bool)
               -> (forall t .
                      (MonadTrans t, Monad (t m))
                   => (n -> InstF f n -> t m Bool)
                   -> (n -> n -> t m Bool)
                   -> t m Bool)
               -> m Bool
tieKnotCompare cmp start = evalStateT (start qa qb) (S.empty, S.empty)
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


nCompare :: (MCVT m n ~ InstF f n, MCR m n, Ord n, Ord f --,
             -- m' ~ StateT (S.Set (v,InstF f v), S.Set (v,v)) m
            )
         => (forall m' .
                (Monad m')
             => (n -> InstF f n -> m' Bool)
             -> (n -> n -> m' Bool)
             -> InstF f n -> InstF f n -> m' Bool)
         -> n -> n -> m Bool
nCompare cmp i0 j0 = tieKnotCompare cmp (\_ qb -> qb i0 j0)

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

nLeqGLB, nSubGLB :: (Ord f, Ord n,
                     MonadFix m, MCVT m n ~ InstF f n, MCR m n, MCN m n)
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
--     1. both inputs are not free variables (a free variable turns
--        unification into assignment; two makes it aliasing)
--     2. either input represents more than one possible term
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
--
-- This function returns the key given as a convenient shorthand.
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
            MCVT m u ~ UR f n k u, MCD m u, MCR m u)
        => Bool -> u -> u -> m k
unifyUU _ a b | a == b = cdelete a >>= aliasW -- XXX probably should be error
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
--
-- XXX We probably do not handle free-free unification correctly, in light
-- of §5.4.1.  For the moment, I am skipping this.
unifyVV :: forall f k m n u v .
           (Eq k, Eq u, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCD m k, MCM m k, MCN m k,
            MCVT m n ~ InstF f n, MCN m n, MCR m n,
            MCVT m u ~ UR f n k u, MCD m u, MCR m u,
            MCVT m v ~ VR n k u, MCA m v)
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
unifyNV :: forall f k m n u v .
           (Eq k, Eq u, Ord f, Ord n, Show n, Show k,
            MonadFix m,
            MCVT m k ~ ENKRI f n k, MCA m k, MCM m k,
            MCVT m n ~ InstF f n, MCN m n, MCR m n,
            MCVT m u ~ UR f n k u, MCR m u, MCW m u,
            MCVT m v ~ VR n k u, MCR m v, MCW m v)
        => Bool
        -> n
        -> v
        -> m v
unifyNV l n0 v0 = do
  x0 <- clookup v0
  xu <- unifyNX n0 x0
  cassign v0 xu
  return v0
 where
  unifyNX :: n -> VR n k u -> m (VR n k u)
  unifyNX na (VRName   nb) = liftM VRName   $ unifyNN l na nb
  unifyNX na (VRKey    kb) = liftM VRKey    $ unifyNK l na kb
  unifyNX na (VRStruct ub) = liftM VRStruct $ unifyNU   na ub

  unifyNU :: n -> u -> m u
  unifyNU na ub = do
    wb <- clookup ub
    wu <- unifyNW na wb 
    cassign ub wu
    return ub

  unifyNW :: n -> UR f n k u -> m (UR f n k u)
  unifyNW na ub = do
    ia <- clookup na
    when (not l) $ semidet_clobbered_unify ia ub >>= flip when (fail "UNU SCU")
    iLeqGLB_ (return . VRName) return unifyNX ia ub


------------------------------------------------------------------------}}}
-- Matching                                                             {{{

subNN :: (Ord f, Ord n, Show n, MCVT m n ~ InstF f n, MCR m n)
      => n -> n -> m Bool
subNN a b = XT.traceShow ("SNN",a,b) $ nSub a b

iCompare :: (MCVT m n ~ InstF f n, MCR m n, Ord n, Ord f --,
             -- m' ~ StateT (S.Set (v,InstF f v), S.Set (v,v)) m
            )
         => (forall m' .
                (Monad m')
             => (n -> InstF f n -> m' Bool)
             -> (n -> n -> m' Bool)
             -> InstF f n -> InstF f n -> m' Bool)
         -> InstF f n -> InstF f n -> m Bool
iCompare cmp i0 j0 = tieKnotCompare cmp (\qa qb -> cmp qa qb i0 j0)

subNI n i = XT.traceShow ("SNI",n,i) $ do
  ni <- clookup n
  iCompare iSub_ ni i

subJN j = either subNN subKN j

subJI j = either subNI subKI j

subQI q i = XT.traceShow ("SQI",q,i) $ iSub_ subJI subJN q i

subQN q n = XT.traceShow ("SQN",q,n) $ do
  ni <- clookup n
  iSub_ subJI subJN q ni

subKN k n = XT.traceShow ("SKN",k,n) $ do
  kq <- clookup k
  (either subNN subQN kq) n

subKI k i = XT.traceShow ("SKI",k,i) $ do
  kq <- clookup k
  (either subNI subQI kq) i

subUI u i = XT.traceShow ("SUI",u,i) $ do
  uw <- clookup u
  iSub_ subXI subXN uw i

subXI x i = XT.traceShow ("SXI",x,i) $ do
  case x of
    VRName   xn -> subNI xn i
    VRKey    xk -> subKI xk i
    VRStruct xu -> subUI xu i

subXN :: forall f k m n u .
         (Ord n, Ord f, MonadFix m, Show n, Show f, Show k, Show u,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k,
          MCVT m u ~ UR f n k u, MCR m u)
      => VR n k u -> n -> m Bool
subXN x n = XT.traceShow ("SXN",x,n) $ do
  case x of
    VRName   xn -> subNN xn n
    VRKey    xk -> subKN xk n
    VRStruct xu -> subUN xu n

subUN :: forall f k m n u .
         (Ord n, Ord f, MonadFix m, Show n, Show f, Show k, Show u,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k,
          MCVT m u ~ UR f n k u, MCR m u)
      => u -> n -> m Bool
subUN u n = XT.traceShow ("SUN",u,n) $ do
  uw <- clookup u
  ni <- clookup n
  iSub_ subXI subXN uw ni

subVN :: forall f k m n u v .
         (Ord n, Ord f, MonadFix m, Show n, Show f, Show k, Show u, Show v,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k,
          MCVT m u ~ UR f n k u, MCR m u,
          MCVT m v ~ VR n k u, MCR m v)
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
doCall :: forall f k m n u v .
          (Ord n, Ord f, Eq u, Eq k,
           Show n, Show f, Show k, Show u, Show v,
           MonadFix m, 
           MCVT m n ~ InstF f n, MCR m n, MCN m n,
           MCVT m k ~ ENKRI f n k, MCA m k, MCM m k, MCR m k,
           MCVT m u ~ UR f n k u, MCR m u, MCW m u,
           MCVT m v ~ VR n k u, MCR m v, MCW m v)
        => (v -> Bool) -- Liveness predicate
        -> [v]         -- Call with these arguments
        -> [(n, n)]    -- Against this pattern
        -> m Bool
doCall l as0 cs0 = go as0 (map fst cs0)
 where
  go []     []     = goUnify as0 (map snd cs0)
  go (a:as) (c:cs) = andM (subVN a c) (go as cs)
  go _      _      = return False

  goUnify []     []     = return True
  goUnify (a:as) (c:cs) = unifyNV (l a) c a >> goUnify as cs
  goUnify _      _      = return False

------------------------------------------------------------------------}}}
-- Merging                                                              {{{

-- XXX Unimplemented

------------------------------------------------------------------------}}}
