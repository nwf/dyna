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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- XXX doCall
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.Functions (
  -- * Expansion
  expandV, -- expandSharedV,
  -- * Unification
  unifyVV, unifyUnaliasedNV, unifyVF,
  -- * Ordering
  leqVV,
  -- * Matching,
  subVN,
  -- * Modes
  doCall,
  -- * Misc
  leqXX, -- leqMXX,
) where

import           Control.Applicative
-- import           Control.Exception
import           Control.Lens
import           Control.Monad.Error.Class
import           Control.Monad.Reader
-- import           Control.Monad.Reader.Class
-- import           Control.Monad.State
-- import           Control.Monad.Trans.Either
-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.RWS
-- import           Data.Functor.Identity
import qualified Data.Map                          as M
-- import qualified Data.Maybe                        as MA
-- import qualified Data.Set                          as S
-- import qualified Data.Traversable                  as T
import           Dyna.Analysis.Mode.Execution.Context
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
ts :: Show a => a -> b -> b
ts = const id -- XT.traceShow

------------------------------------------------------------------------}}}
-- Variable Expansion                                                   {{{

type ExpC m f n k = (Ord f, Show f, Functor m,
                     Monad m,
                     MCVT m k ~ ENKRI f n k, MCR m k,
                     n ~ NIX f)

-- | Deeply extract from the context as per definition 5.3.3, p95
expandV :: (ExpC m f n k, MCVT m DVar ~ VR f n k, MCR m DVar)
        => DVar -> m n
expandV v = clookup v >>= \x ->
            case x of
              VRName n -> return n
              VRStruct _ -> expandX x
              VRKey k -> clookup k
                         >>= either return (expandX . VRStruct . q2y)
 where
  expandX x = nDeep rec x
   where
    rec :: (MCR m k, MCVT m k ~ ENKRI f n k, MonadTrans t, Monad (t m))
        => VR f n k
        -> Int
        -> t m (Either n (InstF f (Either a1 (VR f n k))))
    rec (VRName n)    _ = return (Left n)
    rec (VRStruct y') _ = return (Right $ fmap Right y')
    rec (VRKey k)     _ = do
      e <- lift (clookup k)
      return $ either Left (Right . fmap Right . q2y) e


------------------------------------------------------------------------}}}
-- Leq                                                                  {{{

type LeqC m f n k = (Ord f, Show f, Show k,
                      Monad m,
                      MCVT m k ~ ENKRI f n k, MCR m k,
                      n ~ NIX f)

leqVV :: (LeqC m f n k,
          MCVT m DVar ~ VR f n k, MCR m DVar)
      => DVar -> DVar -> m Bool
leqVV vl vr = do
  xl <- clookup vl
  xr <- clookup vr
  leqXX xl xr

-- | Compare two variable recursions, without write-back side-effects.
--
-- This is the direct lifting of NoContextFunctions.leqXX to the aliasing
-- case.  It is emphatically not definition 5.3.6, p96.  (See 'leqMXX').
--
-- I am not sure this is useful, but it is written mostly as a warm-up.
--
-- XXX We should be testing that this is equivalent to expandV-ing both and
-- calling nLeq, but that depends on us having the ability to generate
-- interesting contexts.
leqXX :: (LeqC m f n k)
      => VR f n k -> VR f n k -> m Bool
leqXX (VRName   nl) (VRName   nr) = leqNN nl nr
leqXX (VRName   nl) (VRStruct yr) = leqNY nl yr
leqXX (VRName   nl) (VRKey    kr) = leqNK nl kr
leqXX (VRStruct yl) (VRName   nr) = leqYN yl nr
leqXX (VRStruct yl) (VRStruct yr) = leqYY yl yr
leqXX (VRStruct yl) (VRKey    kr) = leqYK yl kr
leqXX (VRKey    kl) (VRName   nr) = leqKN kl nr
leqXX (VRKey    kl) (VRStruct yr) = leqKY kl yr
leqXX (VRKey    kl) (VRKey    kr) = leqKK kl kr

leqKK :: (LeqC m f n k) => k -> k -> m Bool
leqKK kl kr = do
  el <- clookup kl
  er <- clookup kr
  case (el, er) of
    (Left  nl, Left  nr) -> leqNN nl nr
    (Left  nl, Right qr) -> leqNQ nl qr
    (Right ql, Left  nr) -> leqQN ql nr
    (Right ql, Right qr) -> leqQQ ql qr

leqKN :: (LeqC m f n k) => k -> n -> m Bool
leqKN kl nr = clookup kl >>= either (flip leqNN nr) (flip leqQN nr)

leqNK :: (LeqC m f n k) => n -> k -> m Bool
leqNK nl kr = clookup kr >>= either (leqNN nl) (leqNQ nl)

leqKY :: (LeqC m f n k) => k -> InstF f (VR f n k) -> m Bool
leqKY kl yr = clookup kl >>= either (flip leqNY yr) (flip leqQY yr)

leqYK :: (LeqC m f n k) => InstF f (VR f n k) -> k -> m Bool
leqYK yl kr = clookup kr >>= either (leqYN yl) (leqYQ yl)

-- | Split J on right
-- leqNJ :: (LeqC m f n k) => NIX f -> Either n k -> m Bool
-- leqNJ nl = either (leqNN nl) (leqNK nl)

-- leqXJ :: (LeqC m f n k) => VR f n k -> Either n k -> m Bool
-- leqXJ xl jr = either (leqXX xl . VRName) (leqXX xl . VRKey) jr

-- leqJI :: (LeqC m f n k) => Either n k -> InstF f n -> m Bool
-- leqJI jl ir = either (\nl -> leqNN nl (nHide ir))
--                      (\kl -> leqKN kl (nHide ir))
--                      jl

-- | Split J on left
-- leqJN :: (LeqC m f n k) => Either n k -> n -> m Bool
-- leqJN jl nr = either (flip leqNN nr) (flip leqKN nr) jl

-- leqJX :: (LeqC m f n k) => Either n k -> VR f n k -> m Bool
-- leqJX jl xr = either (flip leqXX xr . VRName) (flip leqXX xr . VRKey) jl

-- leqJY :: (LeqC m f n k) => Either n k -> InstF f (VR f n k) -> m Bool
-- leqJY jl yr = either (flip leqNY yr) (flip leqKY yr) jl

-- | Induction hypothesis NY
--
-- Note that the induction on this branch is funny: if the 'InstF' is not
-- recursive, we immediately bail out to name-on-name comparison with
-- 'nShallow', rather than expanding the named inst with 'nExpose'.  Since
-- the right hand side is not getting smaller in such a case, the latter
-- strategy would lead divergence.
leqNY :: forall m f n k . (LeqC m f n k) => n -> InstF f (VR f n k) -> m Bool
leqNY nl (nShallow -> Just nr) = leqNN nl nr
leqNY nl yr                    = iLeq_ leqNL leqNX (nExpose nl) yr
 where
  leqNL :: NIX f -> m Bool
  leqNL nl' = leqNY nl' yr

-- | Induction hypothesis YN
--
-- Induction is similarly funny as in 'leqNY'
leqYN :: (LeqC m f n k) => InstF f (VR f n k) -> n -> m Bool
leqYN (nShallow -> Just nl) nr = leqNN nl nr
leqYN yl                    nr = iLeq_ (\l' -> leqXN l' nr) leqXN yl (nExpose nr)

-- | Induction hypothesis YY
leqYY :: (LeqC m f n k) => InstF f (VR f n k) -> InstF f (VR f n k) -> m Bool
leqYY l r = iLeq_ (\l' -> leqXX l' (VRStruct r)) leqXX l r

-- leqXL :: (LeqC m f n k) => VR f n k -> (forall i_ . InstF f i_) -> m Bool
-- leqXL xl yr = leqXX xl (VRStruct yr)

{-
-- | Repackage Y as X and invoke generic dispatch.  Despite the growth of
-- the RHS, this is guaranteed to then step to one of the induction
-- hypotheses, which will reduce both sides.
leqXY :: (LeqC m f n k) => VR f n k -> InstF f (VR f n k) -> m Bool
leqXY xl yr = leqXX xl (VRStruct yr)
-}

{-
leqXI :: (LeqC m f n k) => VR f n k -> InstF f n -> m Bool
leqXI xl ir = leqXX xl (VRName $ nHide ir)
-}

leqXN :: (LeqC m f n k) => VR f n k -> n -> m Bool
leqXN xl nr = leqXX xl (VRName nr)

-- leqXQ :: (LeqC m f n k) => VR f n k -> InstF f (Either n k) -> m Bool
-- leqXQ xl qr = leqXX xl (VRStruct $ q2y qr)

-- | Repackage N as X and invoke generic dispatch.
leqNX :: (LeqC m f n k) => NIX f -> VR f n k -> m Bool
leqNX nl xr = leqXX (VRName nl) xr

-- | Repackage Q
leqNQ :: (LeqC m f n k) => n -> InstF f (Either n k) -> m Bool
leqNQ nl qr = leqNY nl (q2y qr)

leqQN :: (LeqC m f n k) => InstF f (Either n k) -> n -> m Bool
leqQN ql nr = leqYN (q2y ql) nr

leqQY :: (LeqC m f n k) => InstF f (Either n k) -> InstF f (VR f n k) -> m Bool
leqQY ql yr = leqYY (q2y ql) yr

leqYQ :: (LeqC m f n k) => InstF f (VR f n k) -> InstF f (Either n k) -> m Bool
leqYQ yl qr = leqYY yl (q2y qr)

leqQQ :: (LeqC m f n k) => InstF f (Either n k) -> InstF f (Either n k) -> m Bool
leqQQ ql qr = leqYY (q2y ql) (q2y qr)

-- | Induction base case NN
leqNN :: (Ord f, Monad m) => NIX f -> NIX f -> m Bool
leqNN nl nr = return $ nLeq nl nr

------------------------------------------------------------------------}}}
-- LeqM                                                                 {{{

{-
type LeqMC m f n k = (Ord f, Show f, Show k,
                      Monad m,
                      MCVT m k ~ ENKRI f n k, MCR m k, MCM m k,
                      n ~ NIX f)


-- | Apply a leq constraint to the binding state
--
-- See definition 5.3.6, p96
--
-- Notice that leqM is defined in terms of unification to handle aliasing.
-- It further may fail if its second argument is an alias key while its
-- first argument is not.
--
-- Annoyingly enough, the thesis is exceptionally unclear here; dealing with
-- the alias case seems to require that we rely on the comutativity of ⋏ .

leqMXX :: (LeqMC m f n k)
       => VR f n k -> VR f n k -> MaybeT m Bool
leqMXX (VRName   nl) (VRName   nr) = leqNN  nl nr
leqMXX (VRName   nl) (VRStruct yr) = leqMNY nl yr
leqMXX (VRName   _ ) (VRKey    _ ) = mzero
leqMXX (VRStruct yl) (VRName   nr) = leqMYN yl nr
leqMXX (VRStruct yl) (VRStruct yr) = leqMYY yl yr
leqMXX (VRStruct _ ) (VRKey    _ ) = mzero
leqMXX (VRKey    kl) (VRName   nr) = leqMKN kl nr
leqMXX (VRKey    kl) (VRStruct yr) = leqMKY kl yr
-- leqMXX (VRKey    kl) (VRKey    kr) = leqMKK kl kr

leqMKN :: (LeqMC m f n k) => VR f n k -> n -> MaybeT m Bool
leqMKN kl nr = lift (cmerge unifyQN kl nr) >> return True

leqMKY :: (LeqMC m f n k) => k -> InstF f (VR f n k) -> MaybeT m Bool
leqMKY kl yr = lift (cmerge unifyQY kl yr) >> return True

-- | Induction hypothesis NY
leqMNY :: (LeqMC m f n k) => n -> InstF f (VR f n k) -> MaybeT m Bool
leqMNY nl (nShallow -> Just nr) = leqNN nl nr
leqMNY nl ir = iLeq_ leqMNY leqMNX (nExpose nl) ir

-- | Induction hypothesis YN
leqMYN :: (LeqMC m f n k) => InstF f (VR f n k) -> n -> MaybeT m Bool
leqMYN (nShallow -> Just nl) nr = leqNN nl nr
leqMYN yl                    nr = iLeq_ leqMXI leqMXN yl (nExpose nr)

leqMXI :: (LeqMC m f n k) => VR f n k -> InstF f n -> MaybeT m Bool
leqMXI xl ir = leqMXX xl (VRName $ nHide ir)

-- | Induction hypothesis YY
leqMYY :: (LeqMC m f n k)
       => InstF f (VR f n k)
       -> InstF f (VR f n k)
       -> MaybeT m Bool
leqMYY = iLeq_ leqMXY leqMXX

leqMNX :: (LeqMC m f n k) => n -> VR f n k -> MaybeT m Bool
leqMNX nl xr = leqMXX (VRName nl) xr

leqMXN :: (LeqMC m f n k) => VR f n k -> n -> MaybeT m Bool
leqMXN xl nr = leqMXX xl (VRName nr)

-- | Repackage Y as X
leqMXY :: (LeqMC m f n k) => VR f n k -> InstF f (VR f n k) -> MaybeT m Bool
leqMXY xl yr = leqMXX xl (VRStruct yr)

leqMQY :: (LeqMC m f n k) => InstF f (Either n k) -> InstF f (VR f n k) -> MaybeT m Bool
leqMQY ql yr = leqMYY (q2y ql) yr

-}

------------------------------------------------------------------------}}}
-- Unification                                                          {{{

-- | Constraints common to all unification functions
type UnifC  m f n = (Ord f, Show f,
                       Applicative m, Monad m, MonadError UnifFail m,
                       MonadReader UnifParams m,
                       n ~ NIX f)

-- | Constraints for unification on keyed insts
type UnifKC m f n k = (Show k,
                       MCVT m k ~ ENKRI f n k, MCR m k,
                       MCA m k, MCM m k, MCN m k{-, MCNC k m-})

-- | Variable-on-variable unification.  Ah, finally.
--
-- Based on figure 5.7, p 104.
--
-- XXX We probably do not handle free-free unification correctly, in light
-- of §5.4.1.  For the moment, I am skipping this.
unifyVV :: (UnifC m f n, UnifKC m f n k,
            MCVT m DVar ~ VR f n k, MCA m DVar)
        => DVar -> DVar -> m DVar
unifyVV vl vr = calias (\a b -> liftM VRKey $ unifyXX UUnique a b) vl vr

-- | Unaliased-ply unification.  Notice that the result is always an alias
-- key.
unifyXX :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> VR f n k -> VR f n k -> m k
unifyXX u xa xb = ts ("UXX",u,xa,xb) $ unifyXX_ u xa xb
unifyXX_ :: (UnifC m f n, UnifKC m f n k)
         => Uniq -> VR f n k -> VR f n k -> m k
unifyXX_ u (VRName   na) (VRName   nb) = unifyNN u na nb >>= aliasN
unifyXX_ u (VRName   na) (VRKey    kb) = unifyKN u kb na
unifyXX_ u (VRName   na) (VRStruct yb) = aliasY yb >>= flip (unifyKN u) na
unifyXX_ u (VRKey    ka) (VRName   nb) = unifyKN u ka nb
unifyXX_ u (VRKey    ka) (VRKey    kb) = unifyKK u ka kb
unifyXX_ u (VRKey    ka) (VRStruct yb) = aliasY yb >>= unifyKK u ka
unifyXX_ u (VRStruct ya) (VRName   nb) = aliasY ya >>= flip (unifyKN u) nb
unifyXX_ u (VRStruct ya) (VRKey    kb) = aliasY ya >>= unifyKK u kb
unifyXX_ u (VRStruct ya) (VRStruct yb) = aliasY ya >>= \ka ->
                                        aliasY yb >>= unifyKK u ka

-- | Aliased-ply unification.
--
-- Note that the caller is required to do whatever is necessary to ensure
-- that the resulting 'KR' is interpreted in an aliased context going
-- forward.  That is, we expect to be called by 'calias'.
unifyEE :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> ENKRI f n k -> ENKRI f n k -> m (ENKRI f n k)
unifyEE u (Left na)  (Left  nb) = liftM Left  $ unifyNN u na nb
unifyEE u (Left na)  (Right qb) =               unifyNQ u na qb
unifyEE u (Right qa) (Left  nb) =               unifyNQ u nb qa
unifyEE u (Right qa) (Right qb) = liftM Right $ unifyQQ u qa qb

unifyEQ :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> ENKRI f n k -> KRI f n k -> m (ENKRI f n k)
unifyEQ u (Left na)  qb =               unifyNQ u na qb
unifyEQ u (Right qa) qb = liftM Right $ unifyQQ u qa qb

unifyJJ :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> Either n k -> Either n k -> m (Either n k)
unifyJJ u (Left  na) (Left  nb) = liftM Left  $ unifyNN u na nb
unifyJJ u (Left  na) (Right kb) = liftM Right $ unifyKN u kb na
unifyJJ u (Right ka) (Left  nb) = liftM Right $ unifyKN u ka nb
unifyJJ u (Right ka) (Right kb) = liftM Right $ unifyKK u ka kb

unifyJQ :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> Either n k -> KRI f n k -> m (Either n k)
unifyJQ u (Left  na) qb = unifyNQ' u na qb
unifyJQ u (Right ka) qb = unifyKQ  u ka qb

unifyKK :: (UnifC m f n, UnifKC m f n k) => Uniq -> k -> k -> m k
unifyKK u = calias (unifyEE u)

unifyKN :: (UnifC m f n, UnifKC m f n k) => Uniq -> k -> n -> m k
unifyKN u ka nb = do
  ea <- clookup ka
  ts ("UKN",u,ka,ea,nb) $ unifyKN_ u ka nb
unifyKN_ :: (UnifC m f n, UnifKC m f n k) => Uniq -> k -> n -> m k
unifyKN_ u ka nb = cmerge (\a b -> unifyEE u a (Left b)) ka nb >> return ka

unifyKQ :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> k -> KRI f n k -> m (Either n k)
unifyKQ u ka qb = cmerge (\a b -> unifyEQ u a b) ka qb >> return (Right ka)

-- | Induction hypothesis QN
unifyNQ :: (UnifC m f n, UnifKC m f n k)
        => Uniq -> n -> KRI f n k -> m (ENKRI f n k)
unifyNQ u na (nShallow -> Just nb) = liftM Left $ unifyNN u na nb
unifyNQ u na qb                    = selUnifI >>= \f ->
    f (\u' n' -> unifyNQ' u' n' qb)
      (\u' j' -> unifyJJ  u' (Left na) j')
      (\u' n' j' -> either (liftM Left . unifyNN u' n')
                           (liftM Right . flip (unifyKN u) n')
                           j')
      u (nExpose na) qb
    >>= either throwError (return . Right)

unifyNQ' :: (UnifC m f n, UnifKC m f n k)
         => Uniq -> n -> KRI f n k -> m (Either n k)
unifyNQ' u n q = unifyNQ u n q
                 >>= either (return . Left)
                            (liftM Right . cnew . const . return . Right)

-- | Induction hypothesis QQ
unifyQQ :: forall m f n k .
           (UnifC m f n, UnifKC m f n k)
        => Uniq
        -> KRI f n k
        -> KRI f n k
        -> m (KRI f n k)
unifyQQ u ya yb = selUnifI >>= \f ->
    f (\u' j' -> unifyJQ u' j' yb) (\u' j' -> unifyJQ u' j' ya) unifyJJ u ya yb
    >>= either throwError return
 where
  -- fJQ :: Uniq -> (forall i_. InstF f i_) -> Either n k -> m (Either n k)
  -- fJQ u' q' j' = unifyJQ u' j' q'

-- | Name-on-Name unification, which computes a new name for the result.
--   We assume that the sources will be updated by the caller, if
--   applicable (e.g. 'unifyNK').
unifyNN :: UnifC m f n => Uniq -> n -> n -> m n
unifyNN u a b = do
  f <- selUnifN
  either throwError (return . nUpUniq u) $ f a b

-- | Select between two functions based on our unification parameters.
selUnif_ :: (Monad m, MonadReader UnifParams m)
         => a -> a -> m a
selUnif_ l d = do
  live <- view up_live
  fake <- view up_fake
  return $ if live && not fake then l else d

-- | Select between two 'NIX' unifications based on unification parameters.
selUnifN :: (Ord f, Show f,
             Monad m, MonadReader UnifParams m)
         => m (NIX f -> NIX f -> Either UnifFail (NIX f))
selUnifN = selUnif_ nLeqGLBRL nLeqGLBRD

selUnifI :: (Ord f,
             Applicative m, MonadReader UnifParams m)
         => m (TyILeqGLB_ f m i i' o (m (Either UnifFail (InstF f o))))
selUnifI = selUnif_ iLeqGLBRL_ iLeqGLBRD_

-- | Name-on-Variable unification.  This should not be called on names
-- looked up from the context, as that would omit important updates to the
-- context for alias tracking; instead, only call this on ``freestanding''
-- named insts which are known for other reasons, such as part of a
-- predicate mode.
--
-- This function returns the variable given as a convenient shorthand.
--
-- Based on figure 5.7, p 104.
unifyUnaliasedNV :: forall f k m n .
                    (UnifC m f n, UnifKC m f n k, Applicative m,
                     MCVT m DVar ~ VR f n k, MCR m DVar, MCW m DVar)
                 => n
                 -> DVar
                 -> m DVar
unifyUnaliasedNV n0 v0 = do
  x0 <- clookup v0
  xu <- naUnifyNX UUnique n0 x0
  cassign v0 xu
  return v0
 where
  naUnifyNX :: Uniq -> n -> VR f n k -> m (VR f n k)
  naUnifyNX u na (VRName   nb) = liftM VRName   $ unifyNN u na nb
  naUnifyNX u na (VRKey    kb) = liftM VRKey    $ unifyKN u kb na
  naUnifyNX u na (VRStruct ub) =                  naUnifyNY u na ub

  -- Induction hypothesis NY
  naUnifyNY :: Uniq -> n -> InstF f (VR f n k) -> m (VR f n k)
  naUnifyNY u na (nShallow -> Just nb) = liftM VRName $ unifyNN u na nb
  naUnifyNY u na yb                    = liftM VRStruct $
    selUnifI >>= \f ->
    f (\u' n' -> naUnifyNY u' n' yb)
      (\u' x' -> naUnifyNX u' na x')
      naUnifyNX
      u (nExpose na) yb
    >>= either throwError return

-- | Variable-on-Functor unification.  In our case, since we walk over ANF,
-- where every position has been given a name, we assume the outer functor
-- recurses through a set of variables.
--
-- See definition 3.2.20, p53.
unifyVF :: forall m m' f n k .
           (Ord f, Monad m', m ~ SIMCT m' f, n ~ NIX f, Show f,
            MCVT m k ~ ENKRI f n k,
            MCVT m DVar ~ VR f n k)
        => Bool -> (DVar -> m Bool) -> DVar -> f -> [DVar] -> m DVar
unifyVF fake lf v f vs = do
  vl   <- lf v
  vy   <- clookup v
  vys  <- mapM clookup vs

  let vvys = zip vs vys

  -- Perform a dead unification of the variable's old inst and
  -- bound(unique, f(...)).  This gets us just the join on the lattice.
  ki''  <- runReaderT (unifyXX UUnique vy (VRStruct $ IBound UUnique
                                                       (M.singleton f vys)
                                                       False))
                      (UnifParams False fake)

  i'' <- clookup ki''

  do
   vy'  <- expandV v
   vys' <- mapM expandV vs
   ts ("UVF1",i'',vy',vys') $ return ()

  -- If we arrive here, unification was successful;
  -- now, rip through the results and do the second unification pass.
  (u,vks') <- case i'' of
    Left n'' -> case nExpose n'' of
                    IBound u (M.toList -> [(f',ris)]) False | f == f' -> do
                         x <- go (\u_ n_ x_ -> unifyXX u_ (VRName n_) x_)
                                 vl vvys u ris
                         return (u,x)
                    _ -> dynacPanicStr "unifyVF impossible NIX result"
    Right (IBound u (M.toList -> [(f',ris)]) False) | f == f' -> do
                         x <- go (\u_ e_ x_ -> unifyXX u_ (e2x e_) x_) 
                                 vl vvys u ris 
                         return (u,x)
    _ -> dynacPanicStr "unifyVF impossible result"

  -- And now one last unification
  ki' <- runReaderT (unifyXX UUnique (VRKey ki'')
                       (VRStruct $ IBound u
                                          (M.singleton f $ map VRKey vks')
                                          False))
                    (UnifParams False fake)

  cassign v (VRKey ki')
  sequence_ $ zipWithTails (\v_ k_ -> cassign v_ (VRKey k_))
                           (\_ -> lenpanic)
                           (\_ -> lenpanic)
                           vs vks'

  return v
 where
  go :: forall a b gm .
        (gm ~ ReaderT UnifParams m)
     => (Uniq -> a -> b -> gm k)
     -> Bool -> [(DVar,b)] -> Uniq -> [a] -> m [k]
  go uf vl vvys u ris = sequence $ zipWithTails
                           (\ri (v',oi) -> do
                              l <- lf v'
                              runReaderT (uf u ri oi)
                                         (UnifParams (l && vl) fake))
                           (\_ -> lenpanic)
                           (\_ -> lenpanic)
                           ris vvys

  lenpanic = dynacPanicStr "unifyVF length mismatch"

------------------------------------------------------------------------}}}
-- Matching                                                             {{{

subVN :: forall f k m n .
         (Ord f, Functor m, Monad m, n ~ NIX f, Show f,
          MCVT m k ~ ENKRI f n k, MCR m k,
          MCVT m DVar ~ VR f n k, MCR m DVar)
      => DVar
      -> n
      -> m Bool
subVN vl nr = expandV vl >>= \nl -> return $ nl `nSub` nr

{-
 - An old version of matching that attempted to expand in place.
 -
type SubC  m f n = (Ord f, Show f,
                    Monad m,
                    n ~ NIX f)

-- | Constraints for unification on keyed insts
type SubKC m f n k = (MCVT m k ~ ENKRI f n k, MCR m k)


subNN :: (Ord f, n ~ NIX f, Monad m)
      => n -> n -> m Bool
subNN a b = return $ nSub a b

subVN :: forall f k m n .
         (SubC m f n, SubKC m f n k,
          MCVT m DVar ~ VR f n k, MCR m DVar)
      => DVar
      -> n
      -> m Bool
subVN vl nr = do
  xl <- clookup vl
  case xl of
    VRName   nl -> subNN nl nr
    VRKey    kl -> undefined -- subKN uk n
    VRStruct yl -> undefined -- subYN ui n

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
subNI n i = ts ("SNI",n,i) $ do
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
subQI q i = ts ("SQI",q,i) $ iSub_ subJI subJN q i

subQN :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => KRI f n k -> n -> m Bool
subQN q n = ts ("SQN",q,n) $ do
  ni <- clookup n
  subQI q ni

subKN :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => k -> n -> m Bool
subKN k n = ts ("SKN",k,n) $ do
  kq <- clookup k
  (either subNN subQN kq) n

subKI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => k -> InstF f n -> m Bool
subKI k i = ts ("SKI",k,i) $ do
  kq <- clookup k
  (either subNI subQI kq) i

subYI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => InstF f (VR f n k) -> InstF f n -> m Bool
subYI y i = ts ("SUI",y,i) $ do
  iSub_ subXI subXN y i

subXI :: forall f k m n .
         (Ord f, n ~ NI f, Show f, Show k,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => VR f n k -> InstF f n -> m Bool
subXI x i = ts ("SXI",x,i) $ do
  case x of
    VRName   xn -> subNI xn i
    VRKey    xk -> subKI xk i
    VRStruct xy -> subYI xy i

subXN :: forall f k m n .
         (Ord n, Ord f, Show n, Show f, Show k,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => VR f n k -> n -> m Bool
subXN x n = ts ("SXN",x,n) $ do
  case x of
    VRName   xn -> subNN xn n
    VRKey    xk -> subKN xk n
    VRStruct xu -> subUN xu n

subUN :: forall f k m n .
         (Ord n, Ord f, Show n, Show f, Show k,
          MCVT m n ~ InstF f n, MCR m n,
          MCVT m k ~ ENKRI f n k, MCR m k)
      => InstF f (VR f n k) -> n -> m Bool
subUN u n = ts ("SUN",u,n) $ do
  ni <- clookup n
  iSub_ subXI subXN u ni
-}

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
doCall :: forall f k m m' n .
          (m ~ SIMCT m' f, Monad m',
           UnifC m f n, UnifKC m f n k)
        => (DVar -> Bool)  -- Liveness predicate
        -> DVar -> [DVar]  -- Call with these arguments
        -> QMode n         -- Against this pattern
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

-------------------------------------------------------------------------}}}
