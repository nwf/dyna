-- XXX contribute back to wl-pprint-extras

module Dyna.XXX.PPrint (
  fillList, fillPunct, prefixSD, sepBy, valign
) where

import qualified Data.Foldable         as F
import qualified Data.Traversable      as T
import           Text.PrettyPrint.Free

-- | encloseSep with empty enclosers
sepBy :: Doc e -> [Doc e] -> Doc e
sepBy = encloseSep empty empty

-- | Align and vcat
valign :: F.Foldable f => f (Doc e) -> Doc e
valign = align . vcat

-- | Punctuate and fill out the ribbon.  You almost assuredly want to use
-- 'align' or 'hang' with this.
fillPunct :: (T.Traversable f) => Doc e -> f (Doc e) -> Doc e
fillPunct p l = fillCat (punctuate p l)

-- | Like 'fillPunct' but add list framing and commas and 'align'ment.
fillList :: (T.Traversable f) => f (Doc e) -> Doc e
fillList = brackets . align . fillPunct (comma <> space)

-- | Prefix all lines of a 'SimpleDoc' with a given string
prefixSD :: String -> SimpleDoc e -> SimpleDoc e
prefixSD p = pt . go
 where
  pt = SText (length p) p

  go SEmpty = SEmpty
  go (SChar c s)   = SChar c (go s)
  go (SText i t s) = SText i t (go s)
  go x@(SLine _ SEmpty) = x
  go (SLine i s)   = (SLine i (pt $ go s))
  go (SEffect e s) = SEffect e (go s)
