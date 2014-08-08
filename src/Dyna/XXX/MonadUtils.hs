{-# LANGUAGE Rank2Types #-}

module Dyna.XXX.MonadUtils(
  -- * Data utilities generalizing 'Dyna.XXX.DataUtils'
  mapForallM, mapExistsM, setForallM, setExistsM,
  -- * Control-flow
  timesM,
  -- * Logic utilities
  andM, andM1, orM, orM1, allM, anyM,
  -- * MonadState utilities
  bracketState, incState, readState,
  -- * Monad cache utilities
  trySetCache, tryMapCache,
) where

-- import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map  as M
import           Data.Monoid
import qualified Data.Set  as S

andM :: Monad m => m Bool -> m Bool -> m Bool
andM x y = x >>= flip andM1 y 
{-# INLINABLE andM #-}

andM1 :: Monad m => Bool -> m Bool -> m Bool
andM1 False _ = return False
andM1 True  x = x
{-# INLINABLE andM1 #-}

orM :: Monad m => m Bool -> m Bool -> m Bool
orM x y = x >>= flip orM1 y
{-# INLINABLE orM #-}

orM1 :: Monad m => Bool -> m Bool -> m Bool
orM1 True  _ = return True
orM1 False x = x
{-# INLINABLE orM1 #-}

allM :: Monad m => [m Bool] -> m Bool
allM = foldM andM1 True
{-# INLINABLE allM #-}

anyM :: Monad m => [m Bool] -> m Bool
anyM = foldM orM1 False
{-# INLINABLE anyM #-}

timesM :: Monad m => (a -> m a) -> Int -> a -> m a
timesM f = go
 where
  go n _ | n < 0 = error "timesM negative iteration count"
  go 0 a = return a
  go n a = f a >>= go (n-1)
{-# INLINABLE timesM #-}

mapForallM, mapExistsM :: (Monad m)
                       => (k -> v -> m Bool) -> M.Map k v -> m Bool
mapForallM p m = M.foldrWithKey (\k v -> (andM $ p k v)) (return True ) m
mapExistsM p m = M.foldrWithKey (\k v -> (orM  $ p k v)) (return False) m
{-# INLINABLE mapForallM #-}
{-# INLINABLE mapExistsM #-}

setForallM, setExistsM :: (Monad m)
                       => (e -> m Bool) -> S.Set e -> m Bool
setForallM p m = S.foldr (\e -> (andM $ p e)) (return True ) m
setExistsM p m = S.foldr (\e -> (orM  $ p e)) (return False) m

{-# INLINABLE setForallM #-}
{-# INLINABLE setExistsM #-}

bracketState :: (MonadState s m) => s -> m t -> m (t, s)
bracketState bs m = do
 s <- get
 put bs 
 r <- m
 s' <- get
 put s
 return (r, s')
{-# INLINABLE bracketState #-}

-- | Increment a state field given its lens.  Often you will just want to
-- pass "id" or something similarly simple (frequently much shorter than
-- the type given here!)
incState :: (Enum a, MonadState s m)
         => Optical (->) (->) ((,) a) s s a a
         -> m a
incState = (<<%= succ)
{-# INLINABLE incState #-}

readState :: (MonadState a m) => ReaderT a m b -> m b
readState x = get >>= runReaderT x
{-# INLINABLE readState #-}

-- | Often we want to check a set cache for membership, returning 'mempty'
-- if so, or assume this case and run some action to obtain a result.
--
-- This is used, for example, in cycle-breaking in backward chaining, often,
-- using 'True' for the cycle-break value, we assume the provability of our
-- assumption and continue to look for a counter-argument.  Note that this
-- is rather the opposite of most circular systems, where 'False' would be
-- the cycle-break value.
trySetCache :: (Ord e, MonadState s m)
             => Simple Lens s (S.Set e)
             -> r
             -> (e -> m r)
             -> e
             -> m r
trySetCache l cyc miss e = do
  h <- (uses l $ S.member e)
  if h
   then return cyc
   else l %= S.insert e >> miss e
{-# INLINABLE trySetCache #-}


-- | Like 'trySetCache' but a version that actually, well, caches a value.
-- This depends on us being able to store something into the map before
-- actually recursing and does not update the map while recursing (the
-- callbacks may of course do so at their own risk).
tryMapCache :: (Ord e, MonadState s m)
            => Simple Lens s (M.Map e a)
            -> (e -> m a)                   -- On miss, construct @a@
            -> (e -> a -> m ())             -- Now, given @a@, recurse
            -> e
            -> m a
tryMapCache l mk rec e = use (l . at e) >>= maybe miss return
 where
  miss = mk e >>= \k -> (l . at e .= Just k) >> rec e k >> return k
{-# INLINABLE tryMapCache #-}
