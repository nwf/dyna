---------------------------------------------------------------------------
-- | Class definitions for "context" monads.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Dyna.XXX.MonadContext(
  MCVT, MCA(..), MCD(..), MCF(..), MCM(..), MCNC, MCN(..), MCR(..), MCW(..),
) where

import GHC.Prim (Constraint)
-- import           Control.Monad.Trans
-- import           Control.Monad.Trans.Either

------------------------------------------------------------------------}}}
-- Fine-grained context classes                                         {{{

type family MCVT (m :: * -> *) (k :: *) :: *

-- | The monad @m@ has a readable context of type @k -> v@
class (Monad m) => MCR m k where
  clookup :: k -> m (MCVT m k)

-- | The monad @m@ has a writeable context of type @k -> v@
class (Monad m) => MCW m k where
  cassign :: k -> MCVT m k -> m ()

-- | The monad @m@ is able to merge the assertion @k = v@ into
-- its context, provided a mechanism to merge values.
class (Monad m) => MCM m k where
  cmerge :: (MCVT m k -> b -> m (MCVT m k))
         -> k -> b -> m ()

-- | It is possible to delete keys of type @k@ from the context @m@.
class (Monad m) => MCD m k where
  cdelete :: k -> m (MCVT m k)

{-
class (Monad m) => MCN m k where
  -- XXX OLD FORM cnew :: (MCVT m k) -> m k
  cnew :: (k -> m (MCVT m k)) -> m k
-}

type family MCNC k (m' :: * -> *) :: Constraint

-- | The monad @m@ is able to fabricate new keys given a function
-- which can produce a value from a key.  Likely, an instance of 'MCN'
-- will require 'MonadFix', but not always; that's the purpose of 'MCNC'.
class (Monad m) => MCN m k where
  cnew :: (Monad m', MCNC k m')
       => (forall a . m a -> m' a) -> (k -> m' (MCVT m k)) -> m' k

-- | The monad @m@ is able to generate new entities of type @k@.
class (Monad m) => MCF m k where
  cfresh  :: m k

-- | The monad @m@ understands variable aliasing.
class (Monad m) => MCA m k where
  -- | Canonicalize a key by semipruning
  ccanon  :: k -> m k

  -- | Arbitrarily alias these two keys, given a mechanism to merge
  --   their values.  Returns some name for the resulting aliased
  --   object (which need not be either of the inputs, if they
  --   themselves were aliased, for example).
  calias  :: (MCVT m k -> MCVT m k -> m (MCVT m k)) -> k -> k -> m k

------------------------------------------------------------------------}}}
-- Context Transformers                                                 {{{

{-
type instance MCVT (EitherT e m) k = MCVT m k

instance (MCR m k) => MCR (EitherT e m) k where
  clookup k = lift (clookup k)

instance (MCW m k) => MCW (EitherT e m) k where
  cassign k v = lift (cassign k v)
-}

------------------------------------------------------------------------}}}

