module Dyna.XXX.DataUtils (
  -- * 'Data.List' utilities
  -- ** Argmin/argmax idiom
  argmax, argmin,
  -- ** A more faithful zipWith
  zipWithTails,
  -- * 'Data.Map' utilities
  -- ** Quantification
  mapExists, mapForall,
  -- ** Upsertion
  mapUpsert,
  -- ** Maps of lists
  mapInOrApp, mapMinRepView,
  -- ** Unification-style utilities
  mapSemiprune,
  -- ** Backports
  mergeWithKey,
  -- * 'Data.Set' utilities
  -- ** Quantification
  setExists, setForall

) where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Ord  as O
import qualified Data.Set  as S

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax = L.maximumBy . O.comparing

argmin :: (Ord b) => (a -> b) -> [a] -> a
argmin = L.minimumBy . O.comparing

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

-- | Add @v@ to the list of values at @k@, possibly after creating an empty
-- bucket there.

-- XXX maybe consider generalizing this to any collection type?
mapInOrApp :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
mapInOrApp k v m = M.alter (\mv -> Just $ v:nel mv) k m
 where
  nel Nothing  = []
  nel (Just x) = x

-- | Remove an element of the minimum key
--
-- This lets us use Data.Map as a priority queue,
-- using 'mapInOrApp' for insertion.
mapMinRepView :: (Ord k)
              => M.Map k [v] -> Maybe (v, M.Map k [v])
mapMinRepView m = do
  mv <- M.minViewWithKey m
  case mv of
    ((_,[]),m')   -> mapMinRepView m'
    ((k,x:xs),m') -> return (x, M.insert k xs m')


-- | For all those times one builds a map which may yield non-productive
-- steps of variable-to-variable aliasing.  Note that this function may
-- leave the map with identity mappings, which should be carefully
-- interpreted by the user (probably as a free variable)
mapSemiprune :: (Ord k)
             => (v -> Maybe k)	-- ^ Is this a variable link?
             -> (k -> v)		-- ^ What should we store to indicate
                                -- a pointer to this variable?
             -> k               -- ^ Initial variable
             -> M.Map k v		-- ^ In this map
             -> (k, M.Map k v)	-- ^ (terminus of chain, pruned map)
mapSemiprune q p k m = case M.lookup k m >>= q of
                         Nothing -> (k, m)
                         Just k' -> go (S.singleton k) k'
 where
  go v k' =
    case M.lookup k' m >>= q of
      Nothing                     -> (k', setAll m v k')
      Just k'' | k'' `S.member` v -> (k'', setAll m v k'') -- (M.delete k'' m) (S.delete k'' v) k'')
      Just k''                    -> go (k' `S.insert` v) k''

  setAll m' v k' = M.fromList (map (\x -> (x,p k')) $ S.toList v)
                   `M.union` m'


-- | A generalized version of 'zipWith' that gives access to tail elements
-- as well.
zipWithTails :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipWithTails fb fl fr = go
 where
  go [] [] = []
  go [] (r:rs) = fr r : map fr rs
  go (l:ls) [] = fl l : map fl ls
  go (l:ls) (r:rs) = fb l r : go ls rs


-- | An in-efficient version of map merge, to avoid the need to depend on
-- containers >=0.5, because that breaks people with GHC 7.4.  Sigh.
mergeWithKey :: Ord k => (k -> a -> b -> Maybe c) -> (M.Map k a -> M.Map k c) -> (M.Map k b -> M.Map k c)
             -> M.Map k a -> M.Map k b -> M.Map k c
mergeWithKey f g1 g2 ml mr =
    let xl = g1 (M.difference ml mr)
    in let xr = g2 (M.difference mr ml)
    in let mb = M.mapMaybe id $ M.intersectionWithKey f ml mr
    in M.unions [xl,xr,mb]
