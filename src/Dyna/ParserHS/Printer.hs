---------------------------------------------------------------------------
-- | Print out parsed things

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Dyna.ParserHS.Printer (
	renderParseTerm, renderPragma
) where

import qualified Data.ByteString                  as B
import qualified Data.Map                         as M
-- import qualified Data.Maybe                       as MA
import           Dyna.Analysis.Mode.Inst
import qualified Dyna.Analysis.Mode.InstPretty    as IP
-- import           Dyna.Analysis.Mode.Uniq
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.ParserHS.Types
import           Dyna.Term.TTerm (Annotation(..))
import qualified Text.Parser.Expression     as TP
import           Text.PrettyPrint.Free
import           Text.Trifecta.Rendering (Spanned(..))

------------------------------------------------------------------------}}}
-- Pretty printing parsed output                                        {{{

renderParseTerm :: Spanned Term -> Doc e
renderParseTerm (t0 :~ _) = go (fmap renderParseTerm (unTerm t0))
                            -- <> "@" <> ...
 where
  go :: TermF (Doc e) -> Doc e
  go (TVar x)         = "Va" <+> pretty x
  go (TPrim    p)     = text (show p)
  -- go (TPrimStr s)     = ps s
  go (TPrefix f a)    = "Pr" <+> (text $ show f) <+> parens a
  go (TPostfix f a)   = "Po" <+> (text $ show f) <+> parens a
  go (TInfix f r1 r2) = "In" <+> (text $ show f) <+> ap r1 r2
  go (TFunctor f as)  = "Fu" <+> (text $ show f) <+> tupled as
  go (TAnnot an f)    = "An" <+> parens (pan an) <+> parens f

{-
  ps :: DPrimStructF (Doc e) -> Doc e
  ps (DPCons r1 r2)    = "DPCons" <+> ap r1 r2
  ps (DPMapsTo r1 r2)  = "DPMapsTo" <+> ap r1 r2
  ps (DPTuple rs)      = "DPTuple" <+> tupled rs
  ps (DPWithKey r1 r2) = "DPWithKey" <+> ap r1 r2
-}

  pan (AnnType t)       = "AnnType" <+> t 

  ap r1 r2 = align (parens r1 `above` parens r2)

------------------------------------------------------------------------}}}
-- Printing pragma bodies                                               {{{

renderFixity :: Fixity -> Doc e
renderFixity PFPre  = "pre"
renderFixity PFPost = "post"
renderFixity (PFIn a) = "in" <+> ra a
 where
  ra TP.AssocLeft  = "left"
  ra TP.AssocNone  = "none"
  ra TP.AssocRight = "right"

renderFunctor :: B.ByteString -> Doc e
renderFunctor f = squotes (pretty f)

renderInst :: ParsedInst -> Doc e
renderInst (PIVar v)               = pretty v
renderInst (PIInst IFree)          = "free"
renderInst (PIInst (IAny u))       = "any" <> parens (IP.fullUniq u)
renderInst (PIInst (IUniv u))      = "ground" <> parens (IP.fullUniq u)
renderInst (PIInst (IBound u m b)) =
  "bound" <> brackets (IP.fullUniq u)
          <> (if b then "$base" <> semi else empty)
          <> (encloseSep lparen rparen semi
                 $ map (\(k,v) -> pretty k <> tupled (map renderInst v))
                 $ M.toList m)

renderMode :: ParsedModeInst -> Doc e
renderMode = either renderPNWA renderInst

renderPNWA :: NameWithArgs -> Doc e
renderPNWA (PNWA n as) = pretty n <> tupled (map pretty as)

renderPragma_ :: Pragma -> Doc e
renderPragma_ (PBackchain (f,a)) = "backchain" <+> renderFunctor f
                                               <> char '/'
                                               <> pretty a

renderPragma_ (PDisposDefl s) = "dispos_def" <+> rd s
 where
  rd NDDDyna = "dyna"

renderPragma_ (PDispos p s f as) = "dispos" <+> rp p
                                            <+> rs s
                                            <> renderFunctor f
                                            <> tupled (map ra as)
 where
  rp DTPPrefix  = "pre"
  rp DTPPostfix = "post"
  rp DTPInfix   = "in"
  rp DTPFunctor = ""

  rs SDInherit = empty
  rs SDQuote   = "&"
  rs SDEval    = "*"

  ra ADQuote   = "&"
  ra ADEval    = "*"

renderPragma_ (PIAggr f a ag)  = "iaggr" <+> renderFunctor f
                                         <> char '/'
                                         <> pretty a
                                         <+> pretty ag
                                         <+> empty


renderPragma_ (PInst n i) = "inst" <+> renderPNWA n
                                   <+> renderInst i

{-
renderPragma_ (POperAdd f i n) = "oper" <+> "add"
                                        <+> rf f
                                        <+> pretty i
                                        <+> pretty n
-}

renderPragma_ (POperDel f n) = "oper" <+> "del" <+> renderFixity f <+> pretty n

renderPragma_ (PMode n i o) = "mode" <+> renderPNWA n
                                     <+> renderMode i
                                     <+> renderMode o

renderPragma_ (PRuleIx r) = "ruleix" <+> pretty r

renderPragma :: Pragma -> Doc e
renderPragma = enclose ":-" dot . renderPragma_

------------------------------------------------------------------------}}}

