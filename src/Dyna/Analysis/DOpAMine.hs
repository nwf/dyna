---------------------------------------------------------------------------
-- | Definition and utility functions for the Dyna OPerational Abstract
-- MachINE.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.DOpAMine where

import           Control.Lens
import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Main.Defns
-- import           Dyna.Term.Normalized
import           Dyna.Term.TTerm
import           Dyna.XXX.PPrint
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

data ModedVar = MV
              { _mv_var :: DVar
              , _mv_mi  :: NIX Bool DFunct
              , _mv_mo  :: NIX Bool DFunct
              }
 deriving (Show)
$(makeLenses ''ModedVar)

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.
--
-- The 'bscg' parameter is used to hold on to the backend-specific code
-- generator data in 'OPIter' calls.


-- For modes here (strictly for human understanding; the actual calls may
-- be specializations!), we use
--
--   "-"  for "out" (i.e. "free >> ground"),
--   "+"  for "in"  (i.e. "ground >> ground"),
--   "ao" for "free >> any"
--   "ai" for "any >> any"
--   "?"  indicates that the mode is actually annotated in the opcode

--              Opcode     Out         In          Ancillary        Mode
data DOpAMine bscg =
              -- = OPAsgn     DVar        NTV                       -- -+
              -- | OPCheq     DVar        DVar                      -- ++

              -- | Perform a unification.
              --
              -- The backend is expected to peer at the modes here and
              -- perform some inlining of unification whenever possible.
              --
              -- The Left case of the Either input is to allow easy
              -- unification of contants into allocated free variables.
              --
              -- The Right case here does not carry a full ModedVar,
              -- as the mode of the output variable is sufficient (we are
              -- doing unification, after all), just the input binding state.
                OPUnif     ModedVar    (Either TBase (DVar,NIX Bool DFunct)) Det

              -- | Check that two dvars are not equal.  This is used to
              -- prevent double-counting of hyper-edges when any of their
              -- tails can be made to be the same item by specialization.
              --
              -- XXX While inspired by (Eisner, Goldlust, and Smith 2005),
              -- it's unclear that this is actually what we should be doing.
              -- Oh well, live and learn.
              | OPCkne     DVar        DVar                      -- ++

              -- | Check that the input dvar is an interned representation
              -- of the given functor (and arity as computed from the list
              -- length) and if so, unpack its arguments into those dvars.
              --
              -- The output DVars are expected to be unallocated; the input
              -- must be allocated.
              | OPPeel     [DVar]      DVar        DFunct    Det -- -+

              -- | The reverse of OPPeel.
              --
              -- The ouput DVar is expected to be unallocated; the inputs
              -- must be allocated.
              | OPWrap     DVar        [DVar]      DFunct        -- -+


              -- | Construct the representation of a free variable; that is,
              -- an 'IAny' that happens to be free.
              | OPMkFr     DVar                                  -- ao

              -- | Perform a query
              | OPIter     ModedVar    [ModedVar]  DFunct        -- ??
                                                   Det
                                                   (Maybe bscg)

              -- | Perform an arbitrary evaluation query.  Semantically,
              --
              -- @OPWrap x ys f ; OPIndr z x@ is indistinguishable from
              -- @OPIter (MF z) (map MB ys) f DetSemi Nothing@.
              | OPIndr     DVar        DVar                      -- -+

              -- | Emit (i.e. yield) an answer.  Parameters are the
              --   head, value, rule index, and a list of variables
              --   guaranteed to uniquely (together with the rule index)
              --   identify this particular answer.
              | OPEmit                 DVar DVar RuleIx [DVar]

 deriving (Show)

{- XXX Move DOpAMine to being more functional, rather than a list of
 - opcodes!
 -
 - OPBlock  [DOpAMine bscg]
 - OPOrElse [DOpAMine bscg] -- choice points!
 -}

{- XXX New DOpAMine opcodes for unification support.
 -
 - OPMkFree DVar
 -
 - OPCase   DVar      [(DFunct,           [DVar] -> [DOpAMine bscg])]
 - OPCase2  DVar DVar [(DFunct, [DVar] -> [DVar] -> [DOpAMine bscg])]
 -
 - OPMkAnyFree  DVar
 - OPWrapAny ...
 - OPCaseA  DVar (DVar -> [DOpAMine bscg])
 -               ((DFunct,[DVar]) -> [DOpAMine bscg])
 -
 - OPRec ... ?
 -}

------------------------------------------------------------------------}}}
-- Determinism                                                          {{{

detOfDop :: DOpAMine fbs -> Det
detOfDop x = case x of
               -- OPAsgn _ _       -> Det
               -- OPCheq _ _       -> DetSemi
               OPCkne _ _       -> DetSemi
               OPPeel _ _ _ d   -> d
               OPWrap _ _ _     -> Det
               OPUnif _ _ d     -> d
               OPMkFr _         -> Det
               OPIndr _ _       -> DetSemi
               OPIter _ _ _ d _ -> d
               OPEmit _ _ _ _   -> Det

------------------------------------------------------------------------}}}
-- Rendering                                                            {{{

instance Pretty ModedVar where
  pretty x = pretty (x^.mv_var)
         <> char '@'
         <> parens ((pretty $ x^.mv_mi) <> text ">>" <> (pretty $ x^.mv_mo))

type BackendRenderDopIter bs e =
  ModedVar -> [ModedVar] -> DFunct -> Det -> bs -> Doc e

-- | Given a mechanism for rendering backend-specific information,
-- pretty-print a 'DOpAMine' opcode.
renderDOpAMine :: BackendRenderDopIter bs e -> DOpAMine bs -> Doc e
renderDOpAMine = r
 where
  -- r _ (OPAsgn v n)         = text "OPAsgn" <+> pretty v  <+> pretty n
  -- r _ (OPCheq a b)         = text "OPCheq" <+> pretty a  <+> pretty b
  r _ (OPUnif a (Left b) d)  = text "OPUnif" <+> pretty a
                                             <+> text "constant"
                                             <+> pretty b
                                             <+> text (show d)
  r _ (OPUnif a (Right b) d) = text "OPUnif" <+> pretty a
                                             <+> pretty b
                                             <+> text (show d)
  r _ (OPCkne a b)           = text "OPCkne" <+> pretty a  <+> pretty b
  r _ (OPIndr a b)           = text "OPIndr" <+> pretty a  <+> pretty b
  r _ (OPPeel vs v f d)      = text "OPPeel" <+> pretty vs
                                             <+> pretty v
                                             <+> pretty f
                                             <+> text (show d)
  r _ (OPWrap v vs f)        = text "OPWrap" <+> pretty v
                                             <+> pretty vs <+> pretty f
  r _ (OPMkFr v)             = text "OPMkFr" <+> pretty v
  r e (OPIter v vs f d b)    = text "OPIter"
                               <+> pretty v
                               <+> list (map pretty vs)
                               <+> squotes (pretty f)
                               <+> text (show d)
                               <> maybe empty
                                        ((space <>) . braces . e v vs f d)
                                        b
  r _ (OPEmit h v i vs)      = text "OPEmit"
                               <+> pretty h
                               <+> pretty v
                               <+> pretty i
                               <+> fillList (map pretty vs)

------------------------------------------------------------------------}}}
