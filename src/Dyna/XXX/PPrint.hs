-- XXX contribute back to wl-pprint-extras

module Dyna.XXX.PPrint (
  sepBy, valign
) where

import qualified Data.Foldable         as F
import           Text.PrettyPrint.Free

sepBy :: Doc e -> [Doc e] -> Doc e
sepBy = encloseSep empty empty

valign :: F.Foldable f => f (Doc e) -> Doc e
valign = align . vcat
