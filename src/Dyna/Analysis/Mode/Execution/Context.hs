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
    VV(..), VR(..),

    -- * Context
    -- ** Notes
    -- $context

    -- ** Monad
    SIMCT(..), runSIMCT,
    -- *** And its context
    SIMCtx(..), emptySIMCtx,

    -- ** Internal helper functions
    aliasX, aliasY,
)where

import           Control.Exception(assert)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
-- import           Data.Function
import qualified Data.Map                 as M
import qualified Data.Traversable         as T
-- import           Data.Unique
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Unification
import           Dyna.Main.Exception
import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
import qualified Debug.Trace              as XT
import qualified Text.PrettyPrint.Free    as PP

------------------------------------------------------------------------}}}
-- Inst Types                                                           {{{
-- Insts: Inst Keys (§5.3.1, p94)                                       {{{

-- | An aliased variable, also known as an Inst Key.  See §5.3.1, p94.
--
-- We use 'Int' internally for the moment
newtype KI = KI { unKI :: Int } deriving (Eq,Num,Ord,Show)

-- | Key InstMap Values.  These represent aliased bits of structure built up
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
  -- | An alias chain.  It is safe to semiprune these as desired.
  | KRKey  k
  -- | An aliased inst ply, which recurses either as an (aliased) named inst
  -- or as aliased structure.
  | KRInst (KRI f n k)
 deriving (Eq,Ord,Show)

type KRI f n k = InstF f (Either n k)

-- | When the user looks up a key, they expect to get either a name or a ply
-- of an inst which appropriately recurses ('KRI').  Using this rather than
-- 'KR' directly in our (e.g.) 'MCR' instance ensures that we cannot leak
-- information about aliasing internal to the inst key map.
type ENKRI f n k = Either n (KRI f n k)

------------------------------------------------------------------------}}}
-- Insts: Unaliased Insts                                               {{{

{-
-- | An unaliased variable.  Again we use 'Int' internally for the moment.
newtype UI = UI { unUI :: Int } deriving (Eq,Num,Ord,Show)

-- | Unaliased (User) InstMap Values.  These represent unaliased structure
-- built up during analysis.
--
-- Note that Vars are defined to be acyclic (XXX again, no occurs check
-- right now)
type UR f n k u = InstF f (VR n k u)
-}

------------------------------------------------------------------------}}}
-- Insts: Variables                                                     {{{

-- | An user variable.
newtype VV = VV { unVV :: String } deriving (Eq,Ord,Show)

-- | Variables (and unaliased structure) bindings
data VR f n k =
  -- | Defined named inst (unaliased)
    VRName   n
  -- | Unaliased structure
  | VRStruct (InstF f (VR f n k))
  -- | Aliased structure
  | VRKey    k
 deriving (Eq,Ord,Show)

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Context                                                              {{{
-- Context : Basics                                                     {{{

-- | Simplistic InstMap Context
data SIMCtx f = SIMCtx { _simctx_next_k   :: KI
                       , _simctx_map_k    :: M.Map KI (KR f (NIX f) KI)
                       , _simctx_map_v    :: M.Map VV (VR f (NIX f) KI)
                       }
 deriving (Show)
$(makeLenses ''SIMCtx)

newtype SIMCT m f a = SIMCT { unSIMCT :: StateT (SIMCtx f) (EitherT UnifFail m) a }
 deriving (Monad,MonadFix)

instance (Monad m) => MonadError UnifFail (SIMCT m f) where
  throwError e = SIMCT (lift (left e))
  catchError a f = SIMCT (liftCatch catchError (unSIMCT a) (unSIMCT . f))

instance MonadIO m => MonadIO (SIMCT m f) where
  liftIO m = SIMCT $ lift (liftIO m)

{-
 - XXX maybe
 
instance (Monad m) => MonadState (SIMCtx f) (SIMCT m f) where
  get = SIMCT get
  put = SIMCT . get
  state = SIMCT . state
-}

emptySIMCtx :: SIMCtx f
emptySIMCtx = SIMCtx 0 M.empty M.empty

runSIMCT :: SIMCT m f a -> SIMCtx f -> m (Either UnifFail (a, SIMCtx f))
runSIMCT q x = runEitherT (runStateT (unSIMCT q) x)

------------------------------------------------------------------------}}}
-- Context : Aliased Keys                                               {{{

key_canon :: MonadState (SIMCtx f) m => KI -> m KI
key_canon k = do
  m <- use simctx_map_k
  let (k',m') = mapSemiprune isKey
                             KRKey
                             k
                             m
  simctx_map_k .= m'
  return k'
 where
  isKey (KRKey x) = Just x
  isKey _         = Nothing

key_lookup :: (MonadState (SIMCtx f) m, Show f)
           => KI
           -> m (ENKRI f (NIX f) KI)
key_lookup k = do
  ck <- key_canon k
  m <- use simctx_map_k
  let r = maybe (dynacPanic $ PP.text "Key context miss:"
                              PP.<+> PP.text (show (k,ck)))
                krenkri
              $ M.lookup ck m
  XT.traceShow ("KL",k,ck,r) $ return ()
  return r
 where
  krenkri (KRKey k') = error $ "Key context noncanonical: "
                                 ++ (show (k,k'))
  krenkri (KRName n) = Left n
  krenkri (KRInst i) = Right i

type instance MCVT (SIMCT m f) KI = ENKRI f (NIX f) KI

instance (Show f, Monad m) => MCR (SIMCT m f) KI where
  clookup = SIMCT . key_lookup

instance (Show f, Monad m) => MCD (SIMCT m f) KI where
  cdelete k = XT.traceShow ("KD",k) $ SIMCT $ do
    r <- key_lookup k
    simctx_map_k %= M.delete k
    return r

{-
instance (Show f, Monad m) => MCW (SIMCT m f) KI where
  cassign v q = SIMCT $
    simctx_map_k %= M.insert v (either KRName KRInst q)
-}

instance (Show f, Monad m) => MCM (SIMCT m f) KI where
  cmerge f k v = SIMCT $ do
    ck <- key_canon k
    m <- use simctx_map_k
    maybe (assert (ck == k) $ error $ "Key context miss in merge: "
                                      ++ (show k))
          (\v' -> do
             merged <- unSIMCT $ f (krenkri v') v
             simctx_map_k .= M.insert ck (either KRName KRInst merged) m)
        $ M.lookup ck m
   where
    krenkri (KRKey k') = error $ "Key context noncanonical in merge: "
                                  ++ (show (k,k'))
    krenkri (KRName n) = Left n
    krenkri (KRInst i) = Right i

type instance MCNC KI m = ()
instance (Show f, Monad m) => MCN (SIMCT m f) KI where
  cnew lf f = do
    k <- lf $ SIMCT $ simctx_next_k <+= 1
    q <- f k
    lf $ SIMCT $ simctx_map_k %= M.insert k (either KRName KRInst q)
    return k

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
    simctx_map_k %= M.insert cl (KRKey cr)
    simctx_map_k %= M.insert cr (either KRName KRInst vm)
    return cr

------------------------------------------------------------------------}}}
-- Context : Constructing Aliased Keys                                  {{{

aliasX :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k,
           MCN m k, MCNC k m)
       => VR f n k -> m k
aliasX (VRName n)   = cnew id $ const $ return $ Left n
aliasX (VRKey  k)   = return k
aliasX (VRStruct u) = aliasY u

aliasY :: forall f n k m .
          (Monad m,
           MCVT m k ~ ENKRI f n k,
           MCN m k, MCNC k m)
       => InstF f (VR f n k) -> m k
aliasY u = T.sequence (fmap (liftM Right . aliasX) u)
            >>= cnew id . const . return . Right

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
            => VV
            -> m (VR f (NIX f) KI)
user_lookup v = do
    m <- use simctx_map_v
    r <- maybe (error $ "User variable context miss: " ++ (show v))
               return
             $ M.lookup v m
    XT.traceShow ("VL",v,r) $ return ()
    return r

type instance MCVT (SIMCT m f) VV = VR f (NIX f) KI

instance (Show f, Monad m) => MCR (SIMCT m f) VV where
  clookup = SIMCT . user_lookup

instance (Show f, Monad m) => MCW (SIMCT m f) VV where
  cassign v w = SIMCT $ simctx_map_v %= M.insert v w

instance (Show f, Monad m) => MCA (SIMCT m f) VV where
  ccanon x = return x

  calias f l r = SIMCT $ do
    vl <- user_lookup l
    vr <- user_lookup r
    x  <- unSIMCT $ f vl vr >>= aliasX
    let x' = VRKey x
    simctx_map_v %= M.insert l x'
    simctx_map_v %= M.insert r x'
    return l

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
