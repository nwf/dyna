
module Dyna.Analysis.ANFPretty (printANF) where

import qualified Data.Either                as E
import qualified Data.Set                   as S
import           Dyna.Analysis.ANF
import           Dyna.Term.Normalized
import           Dyna.XXX.PPrint (valign)

import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T
import           Dyna.XXX.Trifecta (prettySpanLoc)

------------------------------------------------------------------------}}}
-- Pretty Printer                                                       {{{

printANF :: Rule -> Doc e
printANF (Rule rix h a result sp _ cruxes) =
          text ";;" <+> prettySpanLoc sp
  `above`
          text ";; index" <+> pretty rix
  `above`
  ( parens $ (pretty a)
            <+> valign [ (pretty h)
                       , parens $ text "evals"  <+> pevs
                       , parens $ text "unifs"  <+> puns
                       , parens $ text "result" <+> (pretty result)
                       ]
  ) <> line
  where
    (evals, unifs) = E.partitionEithers (S.elems cruxes)

    pft :: FDT -> Doc e
    pft (fn,args)     = hsep $ (pretty fn : (map pretty args))

    pnft :: (Int,FDT) -> Doc e
    pnft (n,(f,args))  = parens $ hsep $ (  pretty f <> char '@' <> pretty n 
                                          : (map pretty args))

    pev (CEval n o i)    = parens (pretty o <+> pretty i <> char '@' <> pretty n)
    pev (CCall n o is f) = parens (pretty o <+> pnft (n,(f,is)))

    pun (CStruct o is f) = parens (pretty o  <+> parens (char '&' <+> pft (f,is)))
    pun (CAssign o v   ) = parens (pretty o  <+> parens (equals   <+> pretty v))
    pun (CEquals v1 v2 ) = parens (pretty v1 <+> parens (equals   <+> pretty v2))
    pun (CNotEqu v1 v2 ) = parens (pretty v1 <+> parens (char '!' <+> pretty v2))

    pevs = valign $ map pev evals
    puns = valign $ map pun unifs

------------------------------------------------------------------------}}}
