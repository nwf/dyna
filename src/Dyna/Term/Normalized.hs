---------------------------------------------------------------------------
-- | Normalized Term Representations of various forms

module Dyna.Term.Normalized (
    FDT, EBF, EVF,
) where

-- import qualified Data.ByteString            as B
import           Dyna.Backend.Primitives (DPrimData)
import           Dyna.Term.TTerm

------------------------------------------------------------------------}}}
-- Normalized Term Representations                                      {{{

-- | Flat Dyna Term (that is, a functor over variables)
type FDT = (DFunct,[DVar])

-- | Either a base case or flat term
type EBF = Either DPrimData FDT

-- | Either a variable or a functor of variables)
type EVF = Either DVar FDT

------------------------------------------------------------------------}}}
