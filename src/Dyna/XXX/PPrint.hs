-- XXX contribute back to wl-pprint-extras

module Dyna.XXX.PPrint (
  prefixSD, sepBy, valign
) where

import qualified Data.Foldable         as F
import           Text.PrettyPrint.Free

-- | encloseSep with empty enclosers
sepBy :: Doc e -> [Doc e] -> Doc e
sepBy = encloseSep empty empty

valign :: F.Foldable f => f (Doc e) -> Doc e
valign = align . vcat

-- | Prefix all lines of a 'SimpleDoc' with a given string
prefixSD :: String -> SimpleDoc e -> SimpleDoc e
prefixSD p = pt . go
 where
  pt = SText (length p) p

  go SEmpty = SEmpty
  go (SChar c s)   = SChar c (go s)
  go (SText i t s) = SText i t (go s)
  go x@(SLine i SEmpty) = x
  go (SLine i s)   = (SLine i (pt $ go s))
  go (SEffect e s) = SEffect e (go s)
