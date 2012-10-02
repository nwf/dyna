module Dyna.XXX.MonadUtils(incState) where

import Control.Monad.State

incState :: State Int Int
incState = do
  s <- get
  put $! (s+1)
  return s
