module Dyna.XXX.DataUtils (
  -- * 'Data.Map' utilities
  -- ** Quantification
  mapExists, mapForall,
  -- ** Upsertion
  mapUpsert,
  -- ** Insertion into a map of lists
  mapInOrApp,
  -- * 'Data.Set' utilities
  -- ** Quantification
  setExists, setForall

) where

import qualified Data.Map as M
import qualified Data.Set as S

mapForall, mapExists :: (k -> v -> Bool) -> M.Map k v -> Bool
mapForall p m = M.foldrWithKey (\k v -> (&& p k v)) True  m
mapExists p m = M.foldrWithKey (\k v -> (|| p k v)) False m

setForall, setExists :: (a -> Bool) -> S.Set a -> Bool
setForall p s = S.fold (\e -> (&& p e)) True  s
setExists p s = S.fold (\e -> (|| p e)) False s

-- | Conditional insertion
--
-- @mapUpsert k v m@ attempts to insert @v@ at key @k@ in @m@ and will
-- either return @Right m'@, if @k@ previously had no value or if the old
-- value was '==' to @v@, or @Left v@ if @k@ was occupied by a different
-- value.
mapUpsert :: (Ord k, Eq v)
          => k -> v
          -> M.Map k v
          -> Either v (M.Map k v)
mapUpsert k v m =
 let (mo, m') = M.insertLookupWithKey (\_ _ _ -> v) k v m
     r        = Right m'
 in maybe r (\o -> if o == v then r else Left o) mo

-- XXX maybe consider generalizing this to any collection type?
mapInOrApp :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
mapInOrApp k v m = M.alter (\mv -> Just $ v:nel mv) k m
 where
  nel Nothing  = []
  nel (Just x) = x
