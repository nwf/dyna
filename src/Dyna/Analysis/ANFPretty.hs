---------------------------------------------------------------------------
-- | Pretty-printer for ANF

-- Header material                                                      {{{
module Dyna.Analysis.ANFPretty (renderANF, renderCruxes) where

import           Data.Either
import qualified Data.IntMap                as IM
import qualified Data.Set                   as S
import           Dyna.Backend.Primitives (DPrimData)
import           Dyna.Analysis.ANF
import           Dyna.Term.Normalized
import           Dyna.Term.TTerm
import           Dyna.XXX.PPrint (valign)

import           Text.PrettyPrint.Free
import           Dyna.XXX.Trifecta (prettySpanLoc)

------------------------------------------------------------------------}}}
-- Pretty Printers                                                      {{{

renderANF :: Rule -> Doc e
renderANF (Rule rix h a result sp _ unifs evals) =
          text ";;" <+> prettySpanLoc sp
  `above`
          text ";; index" <+> pretty rix
  `above`
  ( parens $ (pretty a)
            <+> valign [ (pretty h)
                       , parens $ text "evals"  <+> pevs (IM.toAscList evals)
                       , parens $ text "unifs"  <+> puns (S.toList unifs)
                       , parens $ text "result" <+> (pretty result)
                       ]
  ) <> line

renderCruxes :: S.Set (Crux DVar DPrimData) -> Doc e
renderCruxes cs =
 let (es, us) = partitionEithers (S.toList cs)
 in valign
    [ text "evals" <//> indent 2 (pevs es)
    , text "unifs" <//> indent 2 (puns us)
    ]

------------------------------------------------------------------------}}}
-- Internals                                                            {{{

pft :: FDT -> Doc e
pft (fn,args)     = hsep $ (pretty fn : (map pretty args))

pnft :: (Int,FDT) -> Doc e
pnft (n,(f,args))  = parens $ hsep $ (  pretty f <> char '@' <> pretty n 
                                      : (map pretty args))

pev :: Int -> EvalCrux DVar -> Doc e
pev n (CEval o i)    = parens (pretty o <+> pretty i <> char '@' <> pretty n)
pev n (CCall o is f) = parens (pretty o <+> pnft (n,(f,is)))

pevs :: [(Int, EvalCrux DVar)] -> Doc e
pevs = valign . map (uncurry pev)

pun :: (Pretty a) => UnifCrux DVar a -> Doc e
pun (CStruct o is f) = parens (pretty o  <+> parens (char '&' <+> pft (f,is)))
pun (CAssign o v   ) = parens (pretty o  <+> parens (equals   <+> pretty v))
pun (CEquals v1 v2 ) = parens (pretty v1 <+> parens (equals   <+> pretty v2))
pun (CNotEqu v1 v2 ) = parens (pretty v1 <+> parens (char '!' <+> pretty v2))

puns :: (Pretty a) => [UnifCrux DVar a] -> Doc e
puns = valign . map pun

------------------------------------------------------------------------}}}
