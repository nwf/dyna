{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dyna.XXX.MonadUtils(
  -- * Data utilities generalizing 'Dyna.XXX.DataUtils'
  mapForallM, mapExistsM,
  -- * Logic utilities
  andM, andM1, orM, orM1, allM, anyM,
  -- * MonadState utilities
  bracketState, incState,
  -- * Context classes
  MC(..),
) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map  as M

andM :: Monad m => m Bool -> m Bool -> m Bool
andM x y = x >>= flip andM1 y 

andM1 :: Monad m => Bool -> m Bool -> m Bool
andM1 False _ = return False
andM1 True  x = x

orM :: Monad m => m Bool -> m Bool -> m Bool
orM x y = x >>= flip orM1 y

orM1 :: Monad m => Bool -> m Bool -> m Bool
orM1 True  _ = return True
orM1 False x = x

allM :: Monad m => [m Bool] -> m Bool
allM = foldM andM1 True

anyM :: Monad m => [m Bool] -> m Bool
anyM = foldM orM1 False

mapForallM, mapExistsM :: (Monad m)
                       => (k -> v -> m Bool) -> M.Map k v -> m Bool
mapForallM p m = M.foldrWithKey (\k v -> (andM $ p k v)) (return True ) m
mapExistsM p m = M.foldrWithKey (\k v -> (orM  $ p k v)) (return False) m

bracketState :: (MonadState s m) => s -> m t -> m (t, s)
bracketState bs m = do
 s <- get
 put bs 
 r <- m
 s' <- get
 put s
 return (r, bs)

incState :: (Num a, MonadState a m) => m a
incState = do
  s <- get
  put $! (s+1)
  return s

-- | Assert the the monad @m@ has a context of type @k -> v@.
class (Monad m) => MC m k v where
  clookup :: k -> m v
  cassign :: k -> v -> m ()
  cfresh  :: m k
