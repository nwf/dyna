{-# LANGUAGE FlexibleContexts #-}

module Dyna.XXX.MonadUtils(bracketState, incState) where

import Control.Monad.State

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
