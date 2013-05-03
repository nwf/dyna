---------------------------------------------------------------------------
-- | Normalized Term Representations of various forms

module Dyna.Term.Normalized (
    NT(..), FDT, NTV, EBF, ENF, EVF,
) where

-- import qualified Data.ByteString            as B
import           Dyna.Term.TTerm
import qualified Text.PrettyPrint.Free as PP

------------------------------------------------------------------------}}}
-- Normalized Term Representations                                      {{{

-- | A Normalized Term, parametric in the variable case
data NT v = NTVar v
          | NTBase TBase
 deriving (Eq,Ord,Show)

instance PP.Pretty v => PP.Pretty (NT v) where
  pretty (NTVar  v) = PP.pretty v
  pretty (NTBase t) = PP.pretty t

-- | Normalized Term over 'DVar' (that is, either a primitive or a variable)
type NTV = NT DVar

-- | Flat Dyna Term (that is, a functor over variables)
type FDT = (DFunct,[DVar])

-- | Either a base case or flat term
type EBF = Either TBase FDT

-- | Either a variable or a functor of variables)
type EVF = Either DVar FDT

-- | Either a constant, another variable, or a flat Dyna term
type ENF = Either NTV FDT

------------------------------------------------------------------------}}}
