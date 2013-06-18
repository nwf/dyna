---------------------------------------------------------------------------
-- | Basics of actually running the mode system reasoner.

-- XXX when we need to consider expand_shared, we will also do that on the
-- fly.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.Context (
    -- * Inst Types
    -- ** Naming Conventions
    -- $naming

    -- ** Inst Keys (§5.3.1, p94)
    KI(..), KR(..), KRI, ENKRI,
    -- ** Variables
    VR(..),

    -- * Context
    -- ** Notes
    -- $context

    -- ** Monad
    SIMCT(..), runSIMCT,
    -- *** And its context
    SIMCtx(..), emptySIMCtx, allFreeSIMCtx, ctxFromBindings,

    -- ** Internal helper functions
    e2x, q2y, aliasN, aliasV, aliasX, aliasY, kUpUniq,
)where

import           Control.Applicative (Applicative)
import           Control.Arrow (second)
import           Control.Exception(assert)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
-- import qualified Control.Monad.Trans.Maybe     as CMTM
import qualified Control.Monad.Trans.State     as CMTS
import qualified Control.Monad.Trans.Reader    as CMTR
-- import           Data.Function
import qualified Data.Map                      as M
import qualified Data.IntMap                   as IM
import qualified Data.Traversable              as T
-- import           Data.Unique
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import qualified Dyna.Analysis.Mode.InstPretty as IP
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
-- import qualified Debug.Trace              as XT
import qualified Text.PrettyPrint.Free         as PP

------------------------------------------------------------------------}}}
-- Inst Types                                                           {{{
-- Insts: Inst Keys (§5.3.1, p94)                                       {{{

-- | An aliased variable, also known as an Inst Key.  See §5.3.1, p94.
--
-- We use 'Int' internally for the moment
newtype KI = KI { unKI :: Int } deriving (Eq,Ord,Show)

-- XXX Should probably track some notion of "display name" instead...
instance PP.Pretty KI where
  pretty (KI i) = (PP.text "_AK" PP.<> PP.pretty i)

-- | Key Recursion.  These represent aliased bits of structure built up
-- during analysis.
--
-- Periodically, we will attempt to collect structure that is no longer
-- aliased and move it back to the un-aliased map.  As long as something is
-- in here, however, the interpretation should be that it is certainly
-- aliased.
--
-- Inst Keys are defined to be acyclic (though we really should be running
-- an occurs check during analysis XXX)
--
-- See thesis, section 5.3.1
data KR f n k =
  -- | A defined inst (though filtered through the understanding that it
  -- is aliased).
    KRName n
  -- | An aliased inst ply, which recurses either as an (aliased) named inst
  -- or as aliased structure.
  | KRStruct (KRI f n k)
  -- | An alias chain.  It is safe to semiprune these as desired.
  | KRKey  k
 deriving (Eq,Ord,Show)

instance (PP.Pretty f, PP.Pretty n, PP.Pretty k)
      => PP.Pretty (KR f n k) where
  pretty (KRName n)   = PP.pretty n
  pretty (KRStruct e) = IP.compactly PP.pretty (either PP.pretty PP.pretty) e
  pretty (KRKey k)    = PP.pretty k

type KRI f n k = InstF f (Either n k)

-- | When the user looks up a key, they expect to get either a name or a ply
-- of an inst which appropriately recurses ('KRI').  Using this rather than
-- 'KR' directly in our (e.g.) 'MCR' instance ensures that we cannot leak
-- information about aliasing internal to the inst key map.
type ENKRI f n k = Either n (KRI f n k)

------------------------------------------------------------------------}}}
-- Insts: Variables                                                     {{{

-- | Variables (and unaliased structure) bindings
data VR f n k =
  -- | Defined named inst (unaliased)
    VRName   n
  -- | Unaliased structure
  | VRStruct (InstF f (VR f n k))
  -- | Aliased structure
  | VRKey    k
 deriving (Eq,Ord,Show)

instance (PP.Pretty f, PP.Pretty n, PP.Pretty k)
      => PP.Pretty (VR f n k) where
  pretty (VRName n)   = PP.pretty n
  pretty (VRStruct y) = IP.compactly PP.pretty PP.pretty y
  pretty (VRKey k)    = PP.pretty k

-- | Widen from a more restrictive to less restrictive recursion type.
e2x :: Either n k -> VR f n k
e2x = either VRName VRKey

-- | A shorthand which is useful in recursive traversals of 'KR's.
q2y :: InstF f (Either n k) -> InstF f (VR f n k)
q2y = fmap e2x

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Context                                                              {{{
-- Context : Basics                                                     {{{

-- | Simplistic InstMap Context
data SIMCtx f = SIMCtx { _simctx_next_ki_id :: Int
                       , _simctx_map_k      :: IM.IntMap  (KR f (NIX f) KI)
                       -- , _simctx_map_k_refs :: IM.IntMap  [DVar]
                       , _simctx_map_v      :: M.Map DVar (VR f (NIX f) KI)
                       }
 deriving (Show)
$(makeLenses ''SIMCtx)

newtype SIMCT m f a = SIMCT { unSIMCT :: StateT (SIMCtx f) (EitherT UnifFail m) a }
 deriving (Monad,MonadFix,Functor,Applicative)

instance (Monad m) => MonadError UnifFail (SIMCT m f) where
  throwError e = SIMCT (lift (left e))
  catchError a f = SIMCT (CMTS.liftCatch catchError (unSIMCT a) (unSIMCT . f))

instance MonadIO m => MonadIO (SIMCT m f) where
  liftIO m = SIMCT $ lift (liftIO m)

instance (PP.Pretty f) => PP.Pretty (SIMCtx f) where
  pretty (SIMCtx _ km vm) =
    PP.vcat
    [ PP.text "Unaliased variables:"
    , PP.indent 2 $
      PP.vcat $ flip map (M.toAscList vm)
              $ \(v,vr) ->        PP.pretty v
                           PP.<+> PP.text "=>"
                           PP.<+> PP.pretty vr
    , PP.text "Aliased variables (_AK#):"
    , PP.indent 2 $
      PP.vcat $ flip map (IM.toAscList km)
              $ \(k,kr) ->        PP.pretty k
                           PP.<+> PP.text "=>"
                           PP.<+> PP.pretty kr
	]
 
{-
 - XXX maybe
 
instance (Monad m) => MonadState (SIMCtx f) (SIMCT m f) where
  get = SIMCT get
  put = SIMCT . get
  state = SIMCT . state
-}

emptySIMCtx :: SIMCtx f
emptySIMCtx = SIMCtx 0 IM.empty {- IM.empty -} M.empty

-- XXX make take S.Set DVar?
allFreeSIMCtx :: [DVar] -> SIMCtx f
allFreeSIMCtx fs = SIMCtx 0 IM.empty
                 $ M.fromList $ map (\x -> (x, VRStruct IFree)) fs

ctxFromBindings :: [(DVar, NIX f)] -> SIMCtx f
ctxFromBindings = SIMCtx 0 IM.empty . M.fromList . map (second VRName)

runSIMCT :: SIMCT m f a -> SIMCtx f -> m (Either UnifFail (a, SIMCtx f))
runSIMCT q x = runEitherT (runStateT (unSIMCT q) x)

------------------------------------------------------------------------}}}
-- Context : Aliased Keys                                               {{{

key_canon :: MonadState (SIMCtx f) m => KI -> m KI
key_canon k = do
  m <- use simctx_map_k
  let (k',m') = intmapSemiprune isKey
                                (KRKey . KI)
                                (unKI k)
                                m
  simctx_map_k .= m'
  return (KI k')
 where
  isKey (KRKey (KI x)) = Just x
  isKey _              = Nothing

key_lookup :: (MonadState (SIMCtx f) m, Show f)
           => KI
           -> m (ENKRI f (NIX f) KI)
key_lookup k = do
  ck <- key_canon k
  m <- use simctx_map_k
  let r = maybe (dynacPanic $ PP.text "Key context miss:"
                              PP.<+> PP.text (show (k,ck)))
                krenkri
              $ IM.lookup (unKI ck) m
  -- XT.traceShow ("KL",k,ck,r) $ return ()
  return r
 where
  krenkri (KRKey k') = dynacPanicStr $ "Key context noncanonical: "
                                 ++ (show (k,k'))
  krenkri (KRName n) = Left n
  krenkri (KRStruct i) = Right i

type instance MCVT (SIMCT m f) KI = ENKRI f (NIX f) KI

instance (Show f, Monad m) => MCR (SIMCT m f) KI where
  clookup = SIMCT . key_lookup

instance (Show f, Monad m) => MCD (SIMCT m f) KI where
  cdelete k = {- XT.traceShow ("KD",k) $ -} SIMCT $ do
    r <- key_lookup k
    simctx_map_k %= IM.delete (unKI k)
    return r

{-
instance (Show f, Monad m) => MCW (SIMCT m f) KI where
  cassign v q = SIMCT $
    simctx_map_k %= M.insert v (either KRName KRStruct q)
-}


-- XXX I am concerned about side-effects here: the onus is on the provided
-- callback to manage side-effects within simctx_map_k, since we simply
-- clobber its ck entry with the result.  In principle, it might have
-- changed between invocation and return; I'm asserting on this now, and
-- we'll see if it trips. The Ord instance here is for exactly this assert,
-- as it calls == on NIX, which calls 'nEq' which needs Ord f.
instance (Show f, Ord f, Monad m) => MCM (SIMCT m f) KI where
  cmerge f k v = SIMCT $ do
    ck <- key_canon k
    m <- use simctx_map_k
    maybe (assert (ck == k) $ dynacPanicStr $ "Key context miss in merge: "
                                      ++ (show k))
          (\v' -> do
             merged <- unSIMCT $ f (krenkri v') v
             uses (simctx_map_k . at (unKI ck))
                  (flip assert () . (== Just v'))
             simctx_map_k %= IM.insert (unKI ck)
                                       (either KRName KRStruct merged))
        $ IM.lookup (unKI ck) m
   where
    krenkri (KRKey k') = dynacPanicStr $ "Key context noncanonical in merge: "
                                  ++ (show (k,k'))
    krenkri (KRName n) = Left n
    krenkri (KRStruct i) = Right i

-- type instance MCNC KI m = ()
instance (Show f, Monad m) => MCN (SIMCT m f) KI where
  cnew f = do
    k <- SIMCT $ simctx_next_ki_id <+= 1
    q <- f (KI k)
    SIMCT $ simctx_map_k %= IM.insert k (either KRName KRStruct q)
    return (KI k)

instance (Show f, Monad m) => MCA (SIMCT m f) KI where
  ccanon k = SIMCT $ key_canon k

  -- Since everything in this table is already aliased, just do the merge
  -- and make one arbitrarily point at the other.
  calias f l r = SIMCT $ do
    cl <- key_canon l
    cr <- key_canon r
    vl <- key_lookup cl
    vr <- key_lookup cr
    vm <- unSIMCT $ f vl vr
    simctx_map_k %= IM.insert (unKI cl) (KRKey cr)
    simctx_map_k %= IM.insert (unKI cr) (either KRName KRStruct vm)
    return cr

------------------------------------------------------------------------}}}
-- Context : Constructing Aliased Keys                                  {{{

aliasN :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k,
           MCN m k{-, MCNC k m -})
       => n -> m k
aliasN n = cnew $ const $ return $ Left n

aliasX :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k,
           MCN m k{-, MCNC k m -})
       => VR f n k -> m k
aliasX (VRName n)   = aliasN n
aliasX (VRKey  k)   = return k
aliasX (VRStruct u) = aliasY u

aliasY :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k,
           MCN m k{-, MCNC k m -})
       => InstF f (VR f n k) -> m k
aliasY u = T.sequence (fmap (liftM Right . aliasX) u)
            >>= cnew . const . return . Right
aliasV :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k, MCN m k,
           MCVT m DVar ~ VR f n k, MCR m DVar, MCW m DVar)
       => DVar -> m k
aliasV v = do
  x <- clookup v
  k <- aliasX x
  cassign v $ VRKey k
  return k

kUpUniq :: (Ord f, n ~ NIX f, Monad m, MCVT m k ~ ENKRI f n k, MCM m k)
        => Uniq -> k -> m k
kUpUniq u0 k0 = cmerge go k0 u0 >> return k0
 where
   go   (Left n)  u = return $ Left $ nUpUniq u n
   go a@(Right q) u = case iUniq q of
                        Nothing           -> return a
                        Just u' | u' >= u -> return a
                        Just _            -> liftM Right $ (T.mapM (rec u) q)

   rec u (Left n)  = return $ Left $ nUpUniq u n
   rec u (Right k) = kUpUniq u k >> return (Right k)

{-
-- | Called when we are moving a singleton alias key to unaliased structure.
unalias :: forall f n k u m .
         (Monad m,
          MCVT m u ~ UR f n k u,
          MCR m u, MCN m u,
          MCVT m k ~ ENKRI f n k,
          MCR m k, MCD m k)
      => (k -> m Bool)
      -> k
      -> m (Either n u)
unalias s k0 = do
  lk <- cdelete k0
  case lk of
    Left  n -> return $ Left n
    Right r -> liftM Right $ T.sequence (fmap move r) >>= cnew
 where
  move :: Either n k -> m (VR n k u)
  move (Left n)  = return $ VRName n
  move (Right k) = do
    also <- s k
    if also
     then liftM (either VRName VRStruct) $ unalias s k
     else return $ VRKey k
-}

------------------------------------------------------------------------}}}
-- Context : User Variables                                             {{{

user_lookup :: (MonadState (SIMCtx f) m, Show f)
            => DVar
            -> m (VR f (NIX f) KI)
user_lookup v = do
    m <- use simctx_map_v
    let r = maybe (dynacPanicStr $ "User variable context miss: " ++ (show v))
                  id
                $ M.lookup v m
    -- XT.traceShow ("VL",v,r) $ return ()
    return r

type instance MCVT (SIMCT m f) DVar = VR f (NIX f) KI

instance (Show f, Monad m) => MCR (SIMCT m f) DVar where
  clookup = SIMCT . user_lookup

instance (Show f, Monad m) => MCW (SIMCT m f) DVar where
  cassign v w = SIMCT $ simctx_map_v %= M.insert v w

instance (Show f, Monad m) => MCA (SIMCT m f) DVar where
  ccanon x = return x

  calias f l r = SIMCT $ do
    vl <- user_lookup l
    vr <- user_lookup r
    x  <- unSIMCT $ f vl vr >>= aliasX
    let x' = VRKey x
    simctx_map_v %= M.insert l x'
    simctx_map_v %= M.insert r x'
    return l

{-
user_variable_death :: (MonadState (SIMCtx f) m, Show f) => DVar -> m ()
user_variable_death v = do
    m <- use simctx_map_v
    let r = maybe (dynacPanicStr $ "Dying variable not in context: " ++ (show v))
                  id
                $ M.lookup v m
-}


------------------------------------------------------------------------}}}
-- Context : Transformers                                               {{{

-- These instances are used internally to allow us to wrap our workers with
-- different utility parameters.

type instance MCVT (CMTR.ReaderT r (SIMCT m f)) k = MCVT (SIMCT m f) k

instance (MCA (SIMCT m f) k) => MCA (CMTR.ReaderT r (SIMCT m f)) k where
  ccanon k = lift (ccanon k)

  calias f k1 k2 = CMTR.ask >>= \e ->
     lift (calias (\a b -> CMTR.runReaderT (f a b) e) k1 k2)

instance (MCM (SIMCT m f) k) => MCM (CMTR.ReaderT r (SIMCT m f)) k where
  cmerge f k v = CMTR.ask >>= \e ->
     lift (cmerge (\a b -> CMTR.runReaderT (f a b) e) k v)

instance (MCN (SIMCT m f) k) => MCN (CMTR.ReaderT r (SIMCT m f)) k where
  cnew f = CMTR.ask >>= \e ->
     lift (cnew (\a -> CMTR.runReaderT (f a) e))

instance (MCR (SIMCT m f) k) => MCR (CMTR.ReaderT r (SIMCT m f)) k where
  clookup k = lift (clookup k)

instance (MCW (SIMCT m f) k) => MCW (CMTR.ReaderT r (SIMCT m f)) k where
  cassign k v = lift (cassign k v)

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Haddock Sections                                                     {{{
-- Contexts                                                             {{{

-- $context
--
-- We track three sorts of things in our context:
--
--   1. Inst Keys, which are handles to aliased bits of structure.  An Inst
--   Key handle 'KI' binds a 'KR'.
--
--   2. Unaliased structure handles.  Such a handle 'UI' binds a 'UR',
--   which is exactly a ply of an inst which recurses as a named inst, a
--   key, or another bit of unaliased structure.
--
--   3. User variables, which again are bound to either a named inst, a
--   key, or another bit of unaliased structure.
--
-- XXX The context implementation given here is not ideal, but it is
-- hopefully viable.
--
-- Rather than follow a strict interpretation of the thesis, which uses
-- expand/2 and expand'/2 to eliminate user variables (``inst keys''), we
-- implicitly do the expansion at the points where it becomes necessary
-- during comparison.

------------------------------------------------------------------------}}}
-- Notes on Naming Conventions                                          {{{

-- $naming
-- NAMING CONVENTIONS
--
-- We have a lot of types flying around, so let's get a shorthand for some
-- of them.  Despite the proliferation, much of the implementation is
-- type-directed, so it's not so bad.
--
--   * @N@ -- 'NIX' f
--
--   * @I@ -- @'InstF' f ('NI' f)@
--
--   * @K@ -- 'KI'
--
--   * @R@ -- 'KR'
--
--   * @Q@ -- 'KRI' (i.e. @'InstF' f ('Either' ('NI' f) 'KI')@)
--
--   * @E@ -- 'ENKRI' (i.e. @'Either' n ('InstF' f ('Either' ('NI' f) 'KI'))@
--
--   * @J@ -- @'Either' ('NI' f) 'KI'@
--
--   * @V@ -- 'VV'
--
--   * @X@ -- 'VR' f 'NI' 'KI'
--
--   * @Y@ -- @InstF f (VR f 'NI' 'KI')@

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
