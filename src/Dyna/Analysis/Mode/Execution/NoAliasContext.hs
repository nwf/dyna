---------------------------------------------------------------------------
-- | Basics of actually running the mode system reasoner
--
--
-- This is a simplistic context which demonstrates how to perform
-- unification with all the bells and whistles of §3 but without the alias
-- tracking of §5.  It will eventually be disconnected from the main source
-- tree (but may still be built as part of the test harness) but will
-- probably linger on as a way to attain understanding.


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

module Dyna.Analysis.Mode.Execution.NoAliasContext (
    -- * Inst Types
    -- ** Naming Conventions
    -- $naming

    -- ** Variables
    VR(..), vrToNIX,

    -- ** Monad
    SIMCT(..), runSIMCT,
    -- *** And its context
    SIMCtx(..), emptySIMCtx,
)where

-- import           Control.Exception(assert)
import           Control.Lens
-- import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Control.Monad.Trans.State  as CMTS
import qualified Control.Monad.Trans.Reader as CMTR
-- import           Data.Function
import qualified Data.Map                 as M
-- import qualified Data.Traversable         as T
-- import           Data.Unique
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Unification
-- import           Dyna.Main.Exception
import           Dyna.Term.TTerm
-- import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadContext
-- import qualified Debug.Trace              as XT
-- import qualified Text.PrettyPrint.Free    as PP

------------------------------------------------------------------------}}}
-- Variables                                                            {{{

-- | Variables (and unaliased structure) bindings
data VR f n =
  -- | Defined named inst (unaliased)
    VRName   n
  -- | Unaliased structure
  | VRStruct (InstF f (VR f n))
 deriving (Eq,Ord,Show)

-- XXX Boy this is bad
vrToNIX :: VR f (NIX f) -> NIX f
vrToNIX (VRName n) = n
vrToNIX (VRStruct i) = nHide $ fmap vrToNIX i

------------------------------------------------------------------------}}}
-- Context                                                              {{{
-- Context : Basics                                                     {{{

-- | Simplistic InstMap Context
data SIMCtx f = SIMCtx { _simctx_map_v    :: M.Map DVar (VR f (NIX f))
                       }
 deriving (Show)
$(makeLenses ''SIMCtx)

newtype SIMCT m f a = SIMCT { unSIMCT :: StateT (SIMCtx f) (EitherT UnifFail m) a }
 deriving (Monad,MonadFix)

instance (Monad m) => MonadError UnifFail (SIMCT m f) where
  throwError e = SIMCT (lift (left e))
  catchError a f = SIMCT (CMTS.liftCatch catchError (unSIMCT a) (unSIMCT . f))

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
emptySIMCtx = SIMCtx M.empty

runSIMCT :: SIMCT m f a -> SIMCtx f -> m (Either UnifFail (a, SIMCtx f))
runSIMCT q x = runEitherT (runStateT (unSIMCT q) x)

------------------------------------------------------------------------}}}
-- Context : User Variables                                             {{{

user_lookup :: (MonadState (SIMCtx f) m, Show f)
            => DVar
            -> m (VR f (NIX f))
user_lookup v = do
    m <- use simctx_map_v
    r <- maybe (error $ "User variable context miss: " ++ (show v))
               return
             $ M.lookup v m
    -- XT.traceShow ("VL",v,r) $ return ()
    return r

type instance MCVT (SIMCT m f) DVar = VR f (NIX f)

instance (Show f, Monad m) => MCR (SIMCT m f) DVar where
  clookup = SIMCT . user_lookup

instance (Show f, Monad m) => MCW (SIMCT m f) DVar where
  cassign v w = SIMCT $ simctx_map_v %= M.insert v w

-- | This instance is potentially unsafe (see definition 3.2.19, p53 and
-- following prose) as it does not check that there has been no aliasing.
instance (Show f, Monad m) => MCA (SIMCT m f) DVar where
  ccanon x = return x

  calias f l r = SIMCT $ do
    vl <- user_lookup l
    vr <- user_lookup r
    x  <- unSIMCT $ f vl vr
    simctx_map_v %= M.insert l x
    simctx_map_v %= M.insert r x
    return l

------------------------------------------------------------------------}}}
-- Context : Transformers                                               {{{

-- These instances are used internally to allow us to wrap our workers with
-- different utility parameters.

type instance MCVT (CMTR.ReaderT r (SIMCT m f)) k = MCVT (SIMCT m f) k

instance (MCA (SIMCT m f) k) => MCA (CMTR.ReaderT r (SIMCT m f)) k where
  ccanon k = lift (ccanon k)

  calias f k1 k2 = CMTR.ask >>= \e ->
     lift (calias (\a b -> CMTR.runReaderT (f a b) e) k1 k2)

instance (MCR (SIMCT m f) k) => MCR (CMTR.ReaderT r (SIMCT m f)) k where
  clookup k = lift (clookup k)

instance (MCW (SIMCT m f) k) => MCW (CMTR.ReaderT r (SIMCT m f)) k where
  cassign k v = lift (cassign k v)

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Haddock Sections                                                     {{{
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
--   * @V@ -- 'VV'
--
--   * @X@ -- 'VR' f 'NI'
--
--   * @Y@ -- @InstF f (VR f 'NI')@

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
