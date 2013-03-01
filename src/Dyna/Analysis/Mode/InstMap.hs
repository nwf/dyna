---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system (Inst)
--
-- This module contains the functionality for InstMaps, which are maps from
-- keys to openly-recursive InstF plys.
--
-- XXX This module is likely junk given the current implementation effort,
-- but is here for completeness.  It may go away later.

module Dyna.Analysis.Mode.InstMap where

import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Analysis.Mode.Inst

-- | Options for a user variable binding
data UVarF f i u =
  -- | Another user variable; we use this instead of the @alias()@
  -- construction of the thesis.  Note that user-variable bindings are
  -- required to be in the least fixed point (unlike inst bindings).
    VUser u
  -- | An Inst key.
  | VIVar i
  -- | An 'InstF' ply, which may recurse as either a user var @u@ or
  -- an inst key @i@.
  | VInst (InstF f (Either u i))

-- | An InstMap tells us how to interpret variables @v@ used in our insts,
-- which recurse as @i@.
--
-- The intent is that @v@ is a projection of @i@, possibly @i@ itself.
data InstMap f v i =
  -- | As soon as /any/ 'Inst' in the map becomes @not_reached@ 
  -- (see the commentary on 'IBound'), the entire system configuration is
  -- not reachable.
  --
  -- This carries a map of non-@not_reached@ variables and a set of
  -- @not_reached@ values, which must be nonempty.
    IMNotReached (M.Map v (InstF f i)) (S.Set v)

  -- | Most of the time, however, the system state is reachable.
  --
  -- As a result of 'IMNotReached' there is an invariant on the map here,
  -- coded as 'imWellFormed', which is that every 'IBound' constructor in
  -- the map actually has at least one possible disjunct.
  | IM (M.Map v (InstF f i))
 deriving (Show)

-- | The empty 'InstMap'
imEmpty :: InstMap f v i
imEmpty = IM M.empty

-- | The variables explicitly known about in this map
imKeys :: (Ord v) => InstMap f v i -> S.Set v
imKeys (IM m) = M.keysSet m
imKeys (IMNotReached m s) = S.union s (M.keysSet m)

-- | Fetch the binding of a variable
imLookup :: (Ord v, Show v) => v -> InstMap f v i -> InstF f i
imLookup v (IM m)             = maybe (error $ "InstMap miss: " ++ (show v)) id
                              $ M.lookup v m
imLookup _ (IMNotReached _ _) = (iNotReached UUnique)

-- | Assign and manage the notreached invariant
imAssign :: (Ord v) => v -> InstF f i -> InstMap f v i -> InstMap f v i
imAssign v i (IMNotReached m s) | iIsNotReached i = IMNotReached m (S.insert v s)
imAssign v i (IMNotReached m s) = IMNotReached (M.insert v i m) s
imAssign v i (IM m) | iIsNotReached i = IMNotReached m (S.singleton v)
imAssign v i (IM m) = IM (M.insert v i m)

