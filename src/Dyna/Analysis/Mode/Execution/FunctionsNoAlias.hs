---------------------------------------------------------------------------
-- | Execution-oriented aspects of functions we might actually want to
-- call during mode analysis.
--
-- The rules for all of these functions is that we should
-- 
--  * Destruct user-structure, since that is guaranteed to be finite,
--  and eliminate InstF/InstF cases with the primitive calls.
--
--  * When neither arm is user structure, we must find a way to call
--  the NamedInst functions.
--
--

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.FunctionsNoAlias (
  -- * Expansion
  expandV,
  -- * Unification
  unifyVV, unifyVF, unifyUnaliasedNV,
  -- * Matching,
  subVN,
  -- * Modes
  doCall,
  -- * Misc
  leqXX,
) where

-- import           Control.Applicative
-- import           Control.Exception
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
-- import           Control.Monad.Writer.Class
import           Control.Monad.State
import           Control.Monad.Reader
-- import           Control.Monad.Trans.Either
-- import           Control.Monad.Trans.RWS
-- import           Data.Functor.Identity
import qualified Data.Map                          as M
-- import qualified Data.Maybe                        as MA
-- import qualified Data.Set                          as S
-- import qualified Data.Traversable                  as T
import           Dyna.Analysis.Mode.Execution.ContextNoAlias
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
-- import           Dyna.XXX.MonadUtils
-- import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Variable Expansion                                                   {{{

type ExpC m f n = (Ord f, Show f,
                   Monad m, Functor m,
                   n ~ NIX f)

expandV :: (ExpC m f n, MCVT m DVar ~ VR f n, MCR m DVar)
        => DVar -> m n
expandV v = clookup v >>= \x -> case x of
                                  VRName n -> return n
                                  VRStruct y -> nDeep rec y
 where
  rec (VRName n)   = return (Left n)
  rec (VRStruct y) = return (Right y)

------------------------------------------------------------------------}}}
-- Leq                                                                  {{{

type LeqC m f n = (Ord f, Show f,
                   Monad m,
                   n ~ NIX f)

leqXX :: (LeqC m f n)
      => VR f n -> VR f n -> m Bool
leqXX (VRStruct yl) (VRStruct yr) = iLeq_ leqXY leqXX yl yr
leqXX (VRName   nl) (VRStruct yr) = leqNY nl yr
leqXX (VRStruct yl) (VRName   nr) = leqYN yl nr
leqXX (VRName   nl) (VRName   nr) = return $ nLeq nl nr

leqXY :: (LeqC m f n)
      => VR f n -> InstF f (VR f n) -> m Bool
leqXY (VRName   nl) yr = iLeq_ leqNY leqNX (nExpose nl) yr
leqXY (VRStruct yl) yr = iLeq_ leqXY leqXX yl yr

leqNX :: (LeqC m f n)
      => NIX f -> VR f n -> m Bool
leqNX nl (VRName   nr) = return $ nLeq nl nr
leqNX nl (VRStruct yr) = iLeq_ leqNY leqNX (nExpose nl) yr

leqNY :: (LeqC m f n)
      => NIX f -> InstF f (VR f n) -> m Bool
leqNY nl (nShallow -> Just nr) = {- XT.trace "LNYS" $-} return $ nLeq nl nr
leqNY nl ir = {-XT.traceShow ("LNY",nl,ir) $-} iLeq_ leqNY leqNX (nExpose nl) ir

leqYN :: (LeqC m f n)
      => InstF f (VR f n) -> NIX f -> m Bool
leqYN il nr = iLeq_ leqXI leqXN il (nExpose nr)

leqXI :: (LeqC m f n)
      => VR f n -> InstF f (NIX f) -> m Bool
leqXI (VRStruct yl) ir = leqYN yl (nHide ir)
leqXI (VRName   nl) ir = return $ nLeq nl (nHide ir)

leqXN :: (LeqC m f n)
      => VR f n -> NIX f -> m Bool
leqXN (VRName   nl) nr = return $ nLeq nl nr
leqXN (VRStruct yl) nr = leqYN yl nr

{-
leqDVar :: (LeqC m f n)
      => DVar -> DVar -> m Bool
leqDVar vl vr = do
  xl <- clookup vl
  xr <- clookup vr
  leqXX xl xr

leqVX vl xr = do
  xl <- clookup vl
  leqXX xl xr
-}

------------------------------------------------------------------------}}}
-- Unification                                                          {{{

-- XXX Ought to have a MonadWriter to produce
--  the actual opcode sequence for unification!
--  the determinism information
--
-- The former will require that we again modify the core unification logic
-- to not use T.sequence but that we provide a function of type
-- M.Map f [m a] -> m (M.map f [a]).

-- | Constraints common to unification interface functions
type UnifC'  m f n = (Ord f, Show f,
                     Monad m,
                     MonadError UnifFail m,
                     n ~ NIX f)

-- | Constraints common to all unification functions
type UnifC  m f n = (UnifC' m f n,
                     MonadReader UnifParams m)


-- | Name-on-Name unification, which computes a new name for the result.
--   We assume that the sources will be updated by the caller, if
--   applicable.
unifyNN :: UnifC m f n
        => Uniq -> n -> n -> m n
unifyNN u a b = {- XT.traceShow ("NN",a,b) $-} do
  live <- view up_live
  fake <- view up_fake
  either throwError (return . nUpUniq u) $
    let f = if (live && not fake) then nLeqGLBRL else nLeqGLBRD
    in f a b

-- | Variable-on-variable unification.
--
-- Based on definition 3.2.19, p53
--
-- Thesis errata: Definition 3.2.19 says that the SCU check occurs only at
-- the top level, but reading the Mercury implementation (see
-- @compiler/inst_util.m@'s @abstractly_unify_inst@) makes it clear that
-- this check occurs at every level of the structure.  In particular,
-- @abstractly_unify_inst{,_2,_3}@ calls
-- @abstractly_unify_bound_inst_list{,_2}@, which folds
-- @abstractly_unify_inst_list@, which folds
-- @abstractly_unify_inst@.
--
unifyVV :: forall f m n .
           (UnifC m f n,
            MCVT m DVar ~ VR f n, MCR m DVar, MCA m DVar)
        => DVar -> DVar -> m DVar
unifyVV vl vr | vl == vr = return vl
unifyVV vl vr = {- XT.traceShow ("VV", vl, vr) $-} do
  live <- view up_live
  calias (go live) vl vr
 where
  go l = if l then \a b -> unifyXX UUnique a b >>= checkAndReunif
              else unifyXX UUnique

checkAndReunif :: forall f m n .
                  (UnifC m f n)
               => VR f n -> m (VR f n)
checkAndReunif x = {- XT.trace "CAR" $-} do
                    err <- leqXX x (VRStruct $ IAny UUnique)
                    if err
                     then unifyXX UUnique x (VRStruct $ IAny UShared)
                     else throwError UFExDomain

unifyXX :: (UnifC m f n)
        => Uniq -> VR f n -> VR f n -> m (VR f n)
unifyXX u0 (VRName   na) (VRName   nb) = liftM VRName $ unifyNN u0 na nb
unifyXX u0 (VRName   na) (VRStruct yb) = unifyIY u0 (nExpose na) yb
unifyXX u0 (VRStruct ya) (VRName   nb) = unifyIY u0 (nExpose nb) ya
unifyXX u0 (VRStruct ya) (VRStruct yb) = unifyYY u0 ya yb

-- | This is the closest direct analog of @abstractly_unify_inst@.  The
-- other unify functions are all dedicated to type shifting to get back
-- to this one (or 'unifyNN').
unifyYY :: (UnifC m f n)
        => Uniq -> InstF f (VR f n) -> InstF f (VR f n) -> m (VR f n)
unifyYY u0 ya yb = {- XT.traceShow ("YY", ya, yb) $-} do
  live <- view up_live
  fake <- view up_fake
  let f = if (live && not fake) then iLeqGLBRL_ else iLeqGLBRD_
  either throwError (return . VRStruct) =<< f
   (\u x -> return $ xUpUniq u x)
   (\u x -> return $ xUpUniq u x)
   (\u y x -> unifyXY u x y)
   (\u y x -> unifyXY u x y)
   unifyXX
   u0 ya yb

unifyXY :: (UnifC m f n)
        => Uniq -> VR f n -> InstF f (VR f n) -> m (VR f n)
unifyXY u0 (VRStruct ya) yb = {- XT.trace "XY1" $-} unifyYY u0 ya yb
unifyXY u0 (VRName   nl) (nShallow -> Just nr) = {- XT.trace "XY2" $ -} liftM VRName $ unifyNN u0 nl nr
unifyXY u0 (VRName   nl) yr = {- XT.trace "XY3" $-} unifyIY u0 (nExpose nl) yr

unifyNX ::  (UnifC m f n)
        => Uniq -> n -> VR f n -> m (VR f n)
unifyNX u n (VRName  n') = liftM VRName $ unifyNN u n n'
unifyNX u n (VRStruct y) = unifyIY u (nExpose n) y

unifyIY :: (UnifC m f n)
        => Uniq -> InstF f n -> InstF f (VR f n) -> m (VR f n)
unifyIY u0 ia yb = unifyYY u0 (fmap VRName ia) yb

-- XXX Should stop earlier than it does
xUpUniq :: (Ord f, n ~ NIX f) => Uniq -> VR f n -> VR f n
xUpUniq u (VRName   n) = {- XT.trace "XUU1" $ -} VRName   $ nUpUniq u n
xUpUniq u (VRStruct y) = {- XT.trace "XUU2" $ -} VRStruct $ over inst_uniq (max u)
                                  $ fmap (xUpUniq u) y

-- | Name-on-Variable unification.  This should not be called on names
-- looked up from the context, as that would omit important updates to the
-- context.  Instead, only call this on ``freestanding'' named insts which
-- are known for other reasons, such as part of a predicate mode.
--
-- This function returns the variable given as a convenient shorthand.
--
-- Based on figure 5.7, p 104.
unifyUnaliasedNV :: forall f m n.
                    (UnifC m f n,
                     MCVT m DVar ~ VR f n, MCR m DVar, MCW m DVar)
                 => n
                 -> DVar
                 -> m DVar
unifyUnaliasedNV n0 v0 = do
  live <- view up_live
  x0 <- clookup v0
  xu <- (go live) n0 x0
  cassign v0 xu
  return v0
 where
  go l = if l then \a b -> unifyNX UUnique a b >>= checkAndReunif
              else unifyNX UUnique

-- | Variable-on-Functor unification.  In our case, since we walk over ANF,
-- where every position has been given a name, we assume the outer functor
-- recurses through a set of variables.
--
-- See definition 3.2.20, p53.
unifyVF :: forall m f n .
           (UnifC' m f n
           , MCVT m DVar ~ VR f n, MCR m DVar, MCW m DVar)
        => (DVar -> m Bool) -> DVar -> f -> [DVar] -> m DVar
unifyVF lf v f vs = do
  vl   <- lf v
  vy   <- clookup v
  vys  <- mapM clookup vs

  let vvys = zip vs vys

  -- Perform a fake, dead unification of the variable's old inst and
  -- bound(unique, f(...)).  This gets us just the join on the lattice.
  i''  <- runReaderT (unifyXY UUnique vy (IBound UUnique
                                                 (M.singleton f vys)
												 False))
                     (UnifParams False True)

  -- If we arrive here, unification was successful;
  -- now, rip through the results and do the second unification pass.

  (u,vys') <- case i'' of
    VRName n'' -> case nExpose n'' of
                    IBound u (M.toList -> [(f',ris)]) False | f == f' -> do
                         x <- go unifyNX vl vvys u ris
                         return (u,x)
                    _ -> dynacPanicStr "unifyVF impossible NIX result"
    VRStruct (IBound u (M.toList -> [(f',ris)]) False) | f == f' -> do
                         x <- go unifyXX vl vvys u ris 
                         return (u,x)
    _ -> dynacPanicStr "unifyVF impossible result"

  -- Store back into the context

  () <- sequence_ $ zipWith (cassign) vs vys'
  cassign v (VRStruct $ IBound u (M.singleton f vys') False)

  return v
 where
  go :: forall a b m' .
        (m' ~ ReaderT UnifParams m)
     => (Uniq -> a -> b -> m' (VR f (NIX f)))
     -> Bool -> [(DVar,b)] -> Uniq -> [a] -> m [VR f (NIX f)]
  go uf vl vvys u ris = sequence $ zipWithTails
                           (\ri (v',oi) -> do
                              l <- lf v'
                              runReaderT (uf u ri oi >>= checkAndReunif)
                                         (UnifParams (l && vl) True))
                           (\_ -> dynacPanicStr "unifyVF length mismatch")
                           (\_ -> dynacPanicStr "unifyVF length mismatch")
                           ris vvys
------------------------------------------------------------------------}}}
-- Matching                                                             {{{

-- | Constraints common to all subsumption functions
type SubC  m f n = (Ord f, Show f,
                    Monad m,
                    n ~ NIX f)

subNN :: (SubC m f n)
      => n -> n -> m Bool
subNN a b = {- XT.traceShow ("SNN",a,b) $-} return $ nSub a b

subXI :: (SubC m f n)
      => VR f n -> InstF f (NIX f) -> m Bool
subXI (VRName   ln) ri = subNN ln (nHide ri)
subXI (VRStruct ly) ri = subYI ly ri

subYI :: (SubC m f n)
      => InstF f (VR f n) -> InstF f (NIX f) -> m Bool
subYI l r = iSub_ subXI subXN l r

subXN :: (SubC m f n)
      => VR f n -> NIX f -> m Bool
subXN (VRName   ln) rn = subNN ln rn
subXN (VRStruct ly) rn = subYI ly (nExpose rn)

subVN :: forall f m n .
         (SubC m f n,
          MCVT m DVar ~ VR f n, MCR m DVar)
      => DVar
      -> n
      -> m Bool
subVN v n = {- XT.traceShow ("SVN",v,n) $-} do
  vx <- clookup v
  subXN vx n

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
doCall :: forall f m m' n .
          (m ~ SIMCT m' f, Monad m',
           SubC m f n, UnifC' m f n)
        => (DVar -> Bool) -- Liveness predicate
        -> DVar -> [DVar]   -- Call with these arguments
        -> QMode n      -- Against this pattern
        -> m Bool
doCall l r0 as0 (QMode cs0 (rmi,rmo) _) = go (r0:as0) (rmi:map fst cs0)
 where
  go []     []     = goUnify as0 (rmo:map snd cs0)
  go (a:as) (c:cs) = do
   sub <- subVN a c
   if sub
    then go as cs
    else throwError UFExDomain
  go _      _      = return False

  goUnify []     []     = return True
  goUnify (a:as) (c:cs) = runReaderT (unifyUnaliasedNV c a)
                                     (UnifParams (l a) True)
                          >> goUnify as cs
  goUnify _      _      = return False

------------------------------------------------------------------------}}}
-- Merging                                                              {{{

-- XXX Unimplemented


------------------------------------------------------------------------}}}
