module Dyna.XXX.MonadUtils(
  -- * Data utilities generalizing 'Dyna.XXX.DataUtils'
  mapForallM, mapExistsM, setForallM, setExistsM,
  -- * Logic utilities
  andM, andM1, orM, orM1, allM, anyM,
  -- * MonadState utilities
  bracketState, incState,
) where

-- import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import qualified Data.Map  as M
import qualified Data.Set  as S

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

setForallM, setExistsM :: (Monad m)
                       => (e -> m Bool) -> S.Set e -> m Bool
setForallM p m = S.foldr (\e -> (andM $ p e)) (return True ) m
setExistsM p m = S.foldr (\e -> (orM  $ p e)) (return False) m


bracketState :: (MonadState s m) => s -> m t -> m (t, s)
bracketState bs m = do
 s <- get
 put bs 
 r <- m
 s' <- get
 put s
 return (r, s')

incState :: (Num a, MonadState a m) => m a
incState = id <<%= (+1)
