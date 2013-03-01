---------------------------------------------------------------------------
-- | Class definitions for "context" monads.

-- Header material                                                      {{{
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Dyna.XXX.MonadContext(
  MCVT, MCA(..), MCD(..), MCF(..), MCM(..), MCN(..), MCR(..), MCW(..),
) where

type family MCVT (m :: * -> *) (k :: *) :: *

-- | The monad @m@ has a readable context of type @k -> v@
class (Monad m) => MCR m k where
  clookup :: k -> m (MCVT m k)

-- | The monad @m@ has a writeable context of type @k -> v@
class (Monad m) => MCW m k where
  cassign :: k -> MCVT m k -> m ()

-- | The monad @m@ is able to merge the assertion @k = v@ into
-- its context, provided a mechanism to merge values, if necessary.
class (Monad m) => MCM m k where
  cmerge :: (MCVT m k -> MCVT m k -> m (MCVT m k)) -> k -> (MCVT m k) -> m ()

-- | It is possible to delete keys of type @k@ from the context @m@.
class (Monad m) => MCD m k where
  cdelete :: k -> m (MCVT m k)

-- | The monad @m@ is able to fabricate new keys given only a value.
-- This is likely especially useful when @m@ is 'MonadFix'.
class (Monad m) => MCN m k where
  cnew :: (MCVT m k) -> m k

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
