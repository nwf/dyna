---------------------------------------------------------------------------
-- | Basics of actually running the mode system reasoner.

-- XXX when we need to consider expand_shared, we will also do that on the
-- fly.

-- Header material                                                      {{{
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

module Dyna.Analysis.Mode.Execution.Base (
    -- * Inst Types
    -- ** Naming Conventions
    -- $naming

    -- ** Named Insts
    NI(..), di_unique, di_inst,
    -- ** Inst Keys (§5.3.1, p94)
    KI(..), KR(..), KRI, ENKRI,
    -- ** Unaliased Insts
    UI(..), UR,
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
    aliasW,
)where

import           Control.Exception(assert)
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Function
import qualified Data.Map                 as M
import qualified Data.Traversable         as T
import           Data.Unique
import           Dyna.Analysis.Mode.Inst
import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
import qualified Debug.Trace              as XT

------------------------------------------------------------------------}}}
-- Inst Types                                                           {{{
-- Insts: Named Insts                                                   {{{

-- | A named inst.  These are used when we need recursive Insts.  Notice
-- that they are only permitted to recurse as themselves.  See prose, p60.
--
-- Our implementation relies on globally unique keys created by the runtime
-- system and the use of laziness to tie the knot.  This allows them to be
-- garbage collected when no longer used and means that we do not have to
-- carry around another map in our context.
--
-- Despite this, we continue to provide 'MCR' and 'MCN' instances for named
-- insts, just for uniformity of calling code and ease of changing the
-- underlying representation.
data NI f = NI { _di_unique :: Unique
               , _di_inst   :: InstF f (NI f)
               }
$(makeLenses ''NI)

-- | The 'Eq' instance here is for exact object equality.
instance Eq (NI f) where
  (NI a _) == (NI b _) = a == b

instance Ord (NI f) where
  compare = on compare _di_unique

instance Show (NI f) where
  show (NI u _) = "<NI h=" ++ (show $ hashUnique u) ++ ">"

------------------------------------------------------------------------}}}
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

-- | An unaliased variable.  Again we use 'Int' internally for the moment.
newtype UI = UI { unUI :: Int } deriving (Eq,Num,Ord,Show)

-- | Unaliased (User) InstMap Values.  These represent unaliased structure
-- built up during analysis.
--
-- Note that Vars are defined to be acyclic (XXX again, no occurs check
-- right now)
type UR f n k u = InstF f (VR n k u)

------------------------------------------------------------------------}}}
-- Insts: Variables                                                     {{{

-- | An user variable.
newtype VV = VV { unVV :: String } deriving (Eq,Ord,Show)

-- | Variables (and unaliased structure) bindings
data VR n k u =
  -- | Defined named inst (unaliased)
    VRName   n
  -- | Unaliased structure
  | VRStruct u
  -- | Aliased structure
  | VRKey    k
 deriving (Eq,Ord,Show)

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Context                                                              {{{
-- Context : Basics                                                     {{{
data SIMCtx f = SIMCtx { {- _simctx_next_iv   :: NI
                       , _simctx_map_iv    :: InstMap f NI NI
                       , -}
                         _simctx_next_k :: KI
                       , _simctx_next_u :: UI

                       , _simctx_map_k  :: M.Map KI (KR f (NI f) KI)
                       , _simctx_map_u  :: M.Map UI (UR f (NI f) KI UI)
                       , _simctx_map_v  :: M.Map VV (VR (NI f) KI UI)
                       }
 deriving (Show)
$(makeLenses ''SIMCtx)

newtype SIMCT m f a = SIMCT { unSIMCT :: StateT (SIMCtx f) m a }
 deriving (Monad,MonadFix)

emptySIMCtx :: SIMCtx f
emptySIMCtx = SIMCtx 0 0 M.empty M.empty M.empty

runSIMCT :: SIMCT m f a -> SIMCtx f -> m (a, SIMCtx f)
runSIMCT q x = runStateT (unSIMCT q) x

------------------------------------------------------------------------}}}
-- Context : Named Insts                                                {{{

type instance MCVT (SIMCT m f) (NI f) = InstF f (NI f)

instance (Monad m) => MCR (SIMCT m f) (NI f) where
  clookup (NI _ v) = SIMCT $ return v

instance (MonadIO m) => MCN (SIMCT m f) (NI f) where
  cnew v = SIMCT $ do
    nu <- liftIO $ newUnique
    return $ NI nu v

{-
-- For the old NI definition, which required another map; kept in case we
-- like that better.  A little stale.

instance (Monad m) => MCR (SIMCT m f) NI (InstF f NI) where
  clookup v = SIMCT $ get >>= return . imLookup v . view simctx_map_iv

instance (Monad m) => MCN (SIMCT m f) NI (InstF f NI) where
  cnew v = SIMCT $ do
    k <- simctx_next_iv <+= 1
    simctx_map_iv %= imAssign k v
    return k

instance (Monad m) => MCW (SIMCT m f) NI (InstF f NI) where
  cassign v b = SIMCT $ simctx_map_iv %= (imAssign v b)

instance (Monad m) => MCF (SIMCT m f) NI where
  cfresh = SIMCT $ do
     x <- simctx_next_iv <+= 1
     return x
-}

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
           -> m (ENKRI f (NI f) KI)
key_lookup k = do
  ck <- key_canon k
  m <- use simctx_map_k
  let r = maybe (error $ "Key context miss: " ++ (show (k,ck)))
                krenkri
              $ M.lookup ck m
  XT.traceShow ("KL",k,ck,r) $ return ()
  return r
 where
  krenkri (KRKey k') = error $ "User context noncanonical: "
                                 ++ (show (k,k'))
  krenkri (KRName n) = Left n
  krenkri (KRInst i) = Right i

type instance MCVT (SIMCT m f) KI = ENKRI f (NI f) KI

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
    mm <- maybe (assert (ck == k) $
                   return $ M.insert k (either KRName KRInst v) m)
                (\v' -> do
                   merged <- unSIMCT $ f (krenkri v') v
                   return $ M.insert ck (either KRName KRInst merged) m)
              $ M.lookup ck m
    simctx_map_k .= mm
   where
    krenkri (KRKey k') = error $ "User context noncanonical in merge: "
                                  ++ (show (k,k'))
    krenkri (KRName n) = Left n
    krenkri (KRInst i) = Right i

instance (Show f, Monad m) => MCN (SIMCT m f) KI where
  cnew q = SIMCT $ do
    k <- simctx_next_k <+= 1
    simctx_map_k %= M.insert k (either KRName KRInst q)
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
-- Context : Unaliased                                                  {{{

unaliased_lookup :: (MonadState (SIMCtx f) m, Show f)
                 => UI
                 -> m (UR f (NI f) KI UI)
unaliased_lookup v = do
    m <- use simctx_map_u
    r <- maybe (error $ "Unaliased context miss: " ++ (show v))
               return
             $ M.lookup v m
    return r

type instance MCVT (SIMCT m f) UI = UR f (NI f) KI UI

instance (Show f, Monad m) => MCR (SIMCT m f) UI where
  clookup k = SIMCT $ do
    r <- unaliased_lookup k
    XT.traceShow ("UL",k) $ return r

instance (Show f, Monad m) => MCW (SIMCT m f) UI where
  cassign v w = SIMCT $ simctx_map_u %= M.insert v w

instance (Show f, Monad m) => MCD (SIMCT m f) UI where
  cdelete k = SIMCT $ do
    r <- unaliased_lookup k
    simctx_map_u %= M.delete k
    XT.traceShow ("UD",k,r) $ return r

-- | Move an unaliased structure to the aliased table
aliasW :: forall f n k u m .
          (Monad m,
           MCVT m u ~ UR f n k u,
           MCR m u, MCD m u,
           MCVT m k ~ ENKRI f n k,
           MCN m k)
       => UR f n k u
       -> m k
aliasW x = T.sequence (fmap (liftM Right . aliasX) x) >>= cnew . Right

aliasX :: forall f n k u m .
          (Monad m,
           MCVT m u ~ UR f n k u,
           MCR m u, MCD m u,
           MCVT m k ~ ENKRI f n k,
           MCN m k)
       => VR n k u -> m k
aliasX (VRName n)   = cnew (Left n)
aliasX (VRKey  k)   = return k
aliasX (VRStruct u) = cdelete u >>= T.sequence . fmap (liftM Right . aliasX)
                                >>= cnew . Right

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
            -> m (VR (NI f) KI UI)
user_lookup v = do
    m <- use simctx_map_v
    r <- maybe (error $ "Unaliased context miss: " ++ (show v))
               return
             $ M.lookup v m
    XT.traceShow ("VL",v,r) $ return ()
    return r

type instance MCVT (SIMCT m f) VV = VR (NI f) KI UI

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
--   * @N@ -- 'NI'
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
--   * @U@ -- 'UI'
--
--   * @W@ -- 'UR' (i.e. @'InstF' f ('VR' ('NI' f) 'KI' 'UI')@)
--
--   * @V@ -- 'VV'
--
--   * @X@ -- 'VR' 'NI' 'KI' 'UI'

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
