---------------------------------------------------------------------------
-- | Definition and utility functions for the Dyna OPerational Abstract
-- MachINE.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.DOpAMine where

import           Control.Applicative
import           Control.Lens
import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Backend.Primitives
import qualified Data.ByteString.Char8 as B8
import           Dyna.Main.Defns
import           Dyna.Term.Normalized
import           Dyna.Term.TTerm
import           Dyna.XXX.PPrint
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

data ModedVar = MV
              { _mv_var :: DVar
              , _mv_mi  :: NIX DFunct
              , _mv_mo  :: NIX DFunct
              }
 deriving (Show)
$(makeLenses ''ModedVar)

newtype NamedProcedureId = NPI Int
 deriving (Eq,Ord,Show)

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.
--
-- The 'bscg' parameter is used to hold on to the backend-specific code
-- generator data in 'OPIter' calls.

--              Opcode     Out         In          Ancillary
data DOpAMine bscg
              = OPAsgn     DVar        NTV                       -- -+

              | OPAsnV     DVar        DVar                      -- -+
              | OPAsnP     DVar        DPrimData                 -- -+

              -- | Check that two DVars are equal in order to continue the
              -- program.
              --
              -- This differs from @OPPrimOp $ DPBiCmpEq ...@ in that the
              -- latter returns a value available for later inspection,
              -- while this does not.
              | OPCheq     DVar        DVar                      -- ++

              -- | Check that two dvars are not equal.  This is used to
              -- prevent double-counting of hyper-edges when any of their
              -- tails can be made to be the same item by specialization.
              --
              -- Differs analogously to 'OPCheq' from
              -- @OPPrimOp $ DPBiCmpNe ...@.
              --
              -- XXX While inspired by (Eisner, Goldlust, and Smith 2005),
              -- it's unclear that this is actually what we should be doing.
              -- Oh well, live and learn.
              | OPCkne     DVar        DVar                      -- ++

              -- | Check that the input dvar is an interned representation
              -- of the given functor (and arity as computed from the list
              -- length) and if so, unpack its arguments into those dvars.
              | OPPeel     [DVar]      DVar        DFunct    Det -- -+

              -- | The reverse of OPPeel
              | OPWrap     DVar        [DVar]      DFunct        -- -+

              -- | Perform a query of a built-in function
              | OPPrim     DVar        (DPrimOpF DVar)       Det -- -+

              -- | Perform a query against a user table
              | OPIter     ModedVar    [ModedVar]  DFunct        -- ??
                                                   Det
                                                   (Maybe bscg)

{-
              -- | Call a named procedure
              | OPCall     ModedVar    [ModedVar]  NamedProcedureId -- ??
                                                   Det
-}

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

			  -- | A sequence of operations
			  | OPBloc                             [DOpAMine bscg]

              -- | Lexically scoped temporary variable.
              --
              -- At many points during execution, especially with "implied"
              -- modes, the code generator needs to hypothesize a new
              -- variable.  'OPScop' lets us capture that hypothesis for
              -- satisfaction by the backend code generator.
              --
              -- XXX Maybe this should take an Int and [DVar] -> ... ?
              | OPScop                             (DVar -> DOpAMine bscg)

-- {-# DEPRECATED OPAsgn "use OPAsnP or OPAsnV as required" #-}

mkOPScop :: Int -> ([DVar] -> DOpAMine b) -> DOpAMine b
mkOPScop 0 f = f []
mkOPScop n f = go [] n
 where
  go x 1 = (OPScop $ \v -> f (v:x))
  go x n' = (OPScop $ \v -> go (v:x) (n'-1))

{- 
 - Right now, DOpAMine is entirely right-branching and there is no
 - possibility for case analysis.  A naive case-analysis opcode would have
 - to duplicate the tails of each call (of course, maybe there won't be so
 - many shared-tail programs after all).  I think the right answer is
 - something like 
 -   OPOrEl [DOpAMine d]
 -   OPCut
 - where OPCut eliminates the inner-most choice point.  A traditional
 - case-analysis would then be an OPOrEl in which each branch began by
 - testing the pattern and OPCut after all destruction has succeeded.
 - There might be a better way.
 -}

{- XXX New DOpAMine opcodes for unification support?  This is a sketch, not
 - a finished design.
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
               OPAsgn _ _       -> Det
               OPAsnV _ _       -> Det
               OPAsnP _ _       -> Det
               OPCheq _ _       -> DetSemi
               OPCkne _ _       -> DetSemi
               OPPeel _ _ _ d   -> d
               OPWrap _ _ _     -> Det
               OPIndr _ _       -> DetSemi
               OPIter _ _ _ d _ -> d
               OPEmit _ _ _ _   -> Det
               OPPrim _ _ d     -> d
               -- OPCall _ _ _ d   -> d
               OPBloc ds        -> foldr (detMax . detOfDop) DetErroneous ds
               OPScop f         -> detOfDop $ f ""

------------------------------------------------------------------------}}}
-- Rendering                                                            {{{

instance Pretty ModedVar where
  pretty x = pretty (x^.mv_var)
         <> char '@'
         <> parens ((pretty $ x^.mv_mi) <+> text ">>" <+> (pretty $ x^.mv_mo))

type BackendRenderDopIter bs e =
  ModedVar -> [ModedVar] -> DFunct -> Det -> bs -> Doc e

-- | Given a mechanism for rendering backend-specific information,
-- pretty-print a 'DOpAMine' opcode.
renderDOpAMine :: BackendRenderDopIter bs e -> DOpAMine bs -> Doc e
renderDOpAMine e = r (0 :: Integer)
 where
  r _ (OPAsgn v n)        = text "OPAsgn" <+> pretty v  <+> pretty n
  r _ (OPAsnV v w)        = text "OPAsnV" <+> pretty v  <+> pretty w
  r _ (OPAsnP v p)        = text "OPAsnP" <+> pretty v  <+> pretty p
  r _ (OPCheq a b)        = text "OPCheq" <+> pretty a  <+> pretty b
  r _ (OPCkne a b)        = text "OPCkne" <+> pretty a  <+> pretty b
  r _ (OPIndr a b)        = text "OPIndr" <+> pretty a  <+> pretty b
  r _ (OPPeel vs v f d)   = text "OPPeel" <+> pretty vs
                                          <+> pretty v
                                          <+> pretty f
                                          <+> text (show d)
  r _ (OPWrap v vs f)     = text "OPWrap" <+> pretty v
                                          <+> pretty vs <+> pretty f
  r _ (OPIter v vs f d b) = text "OPIter"
                            <+> pretty v
                            <+> list (map pretty vs)
                            <+> squotes (pretty f)
                            <+> text (show d)
                            <> maybe empty
                                     ((space <>) . braces . e v vs f d)
                                     b
  -- r _ (OPCall v vs n d)   = text "OPCall"
  --                           <+> pretty v
  --                           <+> list (map pretty vs)
  --                           <+> text (show n)
  --                           <+> text (show d)
  r _ (OPEmit h v i vs)   = text "OPEmit"
                            <+> pretty h
                            <+> pretty v
                            <+> pretty i
                            <+> fillList (map pretty vs)
  r _ (OPPrim v pf d)     = text "OPPrim"
                            <+> pretty v
                            <+> pretty pf
                            <+> text (show d)
  r n (OPScop s)          = text "OPScop"
                            <+> text "\\x" <> pretty n
                            <+> text "->"
                            <+> r (n+1) (s (B8.pack $ "x" ++ show n))
  r n (OPBloc ds)         = text "OPBloc" <+> list (r n <$> ds)

instance Show d => Show (DOpAMine d) where
 show dop = show (renderDOpAMine (\_ _ _ _ -> text . show) dop)

------------------------------------------------------------------------}}}
