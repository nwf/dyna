module Dyna.XXX.DataUtils (
  -- * 'Data.Map' utilities
  -- ** Quantification
  mapExists, mapForall,
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
