---------------------------------------------------------------------------
-- | Common, basic definitions of our Analysis modules
--
-- Much of this is pending rework once we get to the mode system of Mercury.

module Dyna.Analysis.Base (
	-- * Normalized Term Representations
	NT(..), FDT, NTV, ENF, EVF,

	-- * Modes
	Mode(..), Moded(..), modeOf, isBound, isFree,
    ModedVar, varOfMV, ModedNT, evnOfMNT, ntvOfMNT,

	-- * DOpAMine
    DOpAMine(..),

	-- * Determinism
	Det(..), detOfDop,
) where

import qualified Data.ByteString            as B
import           Dyna.Term.TTerm
import qualified Text.PrettyPrint.Free as PP

------------------------------------------------------------------------}}}
-- Normalized Term Representations                                      {{{

-- | A Normalized Term, parametric in the variable case
--
-- The Ord instance is solely for Data.Set's use
data NT v = NTNumeric (Either Integer Double)
          | NTString  B.ByteString
          | NTVar     v
 deriving (Eq,Ord,Show)

instance (PP.Pretty v) => PP.Pretty (NT v) where
    pretty (NTNumeric (Left x))  = PP.pretty x
    pretty (NTNumeric (Right x)) = PP.pretty x
    pretty (NTString s)          = PP.dquotes (PP.pretty s)
    pretty (NTVar v)             = PP.pretty v


-- | Normalized Term over 'DVar' (that is, either a primitive or a variable)
type NTV = NT DVar

-- | Flat Dyna Term (that is, a functor over variables)
type FDT = (DFunct,[DVar])

-- | Either a variable or a functor of variables)
type EVF = Either DVar FDT

-- | Either a constant, another variable, or a flat Dyna term
type ENF = Either NTV FDT

------------------------------------------------------------------------}}}
-- Modes                                                                {{{

data Mode = MBound | MFree deriving (Eq,Ord,Show)


data Moded v = MF DVar
             | MB v
 deriving (Eq,Ord,Show)

modeOf :: Moded a -> Mode
modeOf (MF _) = MFree
modeOf (MB _) = MBound

isBound, isFree :: Moded a -> Bool
isBound = (== MBound) . modeOf
isFree  = (== MFree ) . modeOf

type ModedVar = Moded DVar

varOfMV :: ModedVar -> DVar
varOfMV (MF x) = x
varOfMV (MB x) = x

type ModedNT = NT (ModedVar)

evnOfMNT :: ModedNT -> Either DVar NTV
evnOfMNT (NTVar mv)    = case mv of
                           MB v -> Right (NTVar v)
                           MF v -> Left  v
evnOfMNT (NTString s)  = Right (NTString s)
evnOfMNT (NTNumeric n) = Right (NTNumeric n)

ntvOfMNT :: ModedNT -> NTV
ntvOfMNT (NTVar mx)    = NTVar $ varOfMV mx
ntvOfMNT (NTString s)  = NTString s
ntvOfMNT (NTNumeric n) = NTNumeric n

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.

--              Opcode     Out         In          Ancillary
data DOpAMine fbs
              = OPAsgn     DVar        NTV                       -- -+
              | OPCheq     DVar        DVar                      -- ++

              -- | Check that two dvars are not equal.  This is used to
              -- prevent double-counting of hyper-edges when any of their
              -- tails can be made to be the same item by specialization.
              -- 
              -- XXX While inspired by Blatz & Eisner 2006, it's unclear
              -- that this is actually what we should be doing.  Oh well,
              -- live and learn.
              | OPCkne     DVar        DVar                      -- ++

              | OPPeel     [DVar]      DVar        DFunct        -- -+
              | OPWrap     DVar        [DVar]      DFunct        -- -+

              | OPIter     (ModedVar)  [ModedVar]  DFunct        -- ??
                                                   Det
                                                   (Maybe fbs)
              | OPIndr     DVar        DVar                      -- -+
 deriving (Eq,Ord,Show)

------------------------------------------------------------------------}}}
-- Determinism                                                          {{{

data Det = Det          -- ^ Exactly one answer
         | DetSemi      -- ^ At most one answer
         | DetNon       -- ^ Unknown number of answers
 deriving (Eq,Ord,Show)

detOfDop :: DOpAMine fbs -> Det
detOfDop x = case x of
               OPAsgn _ _       -> Det
               OPCheq _ _       -> DetSemi
               OPCkne _ _       -> DetSemi
               OPPeel _ _ _     -> DetSemi
               OPWrap _ _ _     -> Det
               OPIndr _ _       -> DetSemi
               OPIter _ _ _ d _ -> d

------------------------------------------------------------------------}}}
