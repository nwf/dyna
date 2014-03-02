---------------------------------------------------------------------------
-- | Definitions of primitive operations that must be supplied by the
-- backends.
--
-- XXX These ought to be constructed from a table of operations, especially
-- so that we can specify type and mode information.  For the moment, tho',
-- we don't have types and we'll enforce modes manually in the procedure
-- table.

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Dyna.Backend.Primitives where

import           Data.ByteString (ByteString)
import qualified Data.Foldable            as F
import qualified Data.Traversable         as T
import qualified Text.PrettyPrint.Free    as PP

-- | Non-recursive structure
--
-- XXX Do we want to have Bool here or are we content reserving the functors
-- "true/0" and "false/0" to encode those?
data DPrimData =
--    DPBool Bool
    DPDollarNull        -- ^ @$null/0@ for :=
  | DPDouble Double
  | DPDQString ByteString
  | DPInt  Integer
  | DPNil               -- ^ @$nil/0@
 deriving (Eq,Show)

instance PP.Pretty DPrimData where pretty = PP.text . show

-- dynaUnitTerm :: DPrimData
-- dynaUnitTerm = DPBool True

{-
-- | Primitive recursive structures
data DPrimStructF r =
    DPCons r r          -- ^ @$cons/2@
  | DPMapsTo r r        -- ^ @'->'/2@
  | DPTuple [r]         -- ^ @tuple/n@
  | DPWithKey r r		-- ^ @with_key@ for @max=@ and friends
 deriving (Eq,Functor,Show)
-}

-- | Builtin operations of all flavors
data DPrimOpF r =
    DPUnAbs   r
  | DPUnEnum  r     -- ^ Nondeterministic enumeration of the given list.
                    --   (XXX; XREF  DPBiIn)
  | DPUnExp   r
  -- | DPUnKey   r     -- ^ For aggregators which are selectors (e.g. max)
  --                   --   find the currently selected witness.
  | DPUnLNeg  r     -- ^ logical negation
  | DPUnLog   r
  -- XXX | DPUnNew   r     -- ^ Dynabase construction
  | DPUnNNeg  r     -- ^ numeric negation
  | DPUnSqrt  r

  | DPBiAdd   r r
  | DPBiAnd   r r
  | DPBiCmpEq r r	-- ^ Comparison operation yielding boolean
  | DPBiCmpGe r r
  | DPBiCmpGt r r
  | DPBiCmpNe r r
  | DPBiCmpLe r r
  | DPBiCmpLt r r
  | DPBiDiv   r r
  | DPBiExp   r r
  | DPBiIn    r r   -- XXX Should not be a builtin?
  -- XXX | DPBiIs    r r   -- ^ Primitive evaluation let construct
  | DPBiIor   r r
  | DPBiLog   r r   -- (base, argument)
  | DPBiMod   r r
  | DPBiMul   r r
  | DPBiSub   r r
  | DPBiXor   r r
 deriving (Eq,Functor,Show,F.Foldable,T.Traversable)

instance (PP.Pretty r) => PP.Pretty (DPrimOpF r) where
  pretty = PP.text . show . fmap PP.pretty

{-
 -  XXX
-- Aggregators
data DPrimAggr =
    DAAnd
  | DAArb
  | DALast
  | DAMax
  | DAMaj
  | DAMean
  | DAMin
  | DAOr
  | DAProd
  | DAProlog
  | DASum
  | DAUnique
 deriving (Bounded,Eq,Ord,Show)
-}
