module Dyna.XXX.DataUtils (
  -- * 'Data.Map' utilities
  -- ** Quantification
  mapExists, mapForall,
  -- ** Upsertion
  mapUpsert,
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
