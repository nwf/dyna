---------------------------------------------------------------------------

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.RuleModeTest where

import qualified Data.ByteString as B
import Text.PrettyPrint.Free
import Text.Trifecta (Spanned (..))

import Dyna.XXX.PPrint
import Dyna.XXX.TrifectaTest (unsafeParse)

import Dyna.ParserHS.Parser (dlines, Line (..))
import Dyna.Analysis.ANF
import Dyna.Analysis.RuleMode


toANF = runNormalize . normRule

unspan (LRule x :~ _) = x

prettyPlans src =
      let rules = map (toANF.unspan) $ unsafeParse dlines src
          plans = map (planEachEval.snd) $ rules
       in
          show $ (vcat $ zipWith pp plans rules)


-- XXX perhaps "base cases" of the universe (constants) should be interned if
-- they are going to be used in terms. Probably want to skip doubles; Interning
-- integers seems silly but not bad; interning strings seems like a good idea.

pp p ((FRule h a e result), _) = valign $ map f p
   where
     emit = "emit" <+> tupled [pretty h, pretty result]

     f (c@(CFCall f, ns, n), Just plan) = valign [ "def update_" <> pretty f <> "(id, value):"  -- TODO: need unique variable names for id and value
                                               , "#" <+> pcrux c
                                               , (tupled $ map pnt ns) <+> "= load(update_id)"  -- TODO: should be all vars
                                               , pnt n <+> "= value"                            -- TODO: return shouldn't be pnt
                                               , pplan plan
                                               , emit]
     f (crux, Nothing) = error $ "Did not find a plan for " ++ (show $ pcrux crux)

     pplan (_, action) = valign $ map pdope action

     pcrux (CFCall f, ns, n) = pnt n <+> equals <+> pred f ns

     pdope (OPIndirEval _ _) = error "indirect evaluation not implemented"
     pdope (OPAssign v val) = pretty v <+> equals <+> pnt val
     pdope (OPCheck v val) = hsep ["assert", pretty v, "==", pnt val]

     pdope (OPGetArgs vs id) = tupled (map pretty vs) <+> equals <+> "peel" <> (parens $ pretty id)
     pdope (OPCheckFunctor v f a) = "check" <+> pretty f <> tupled [text $ show a, pretty v]

     pdope (OPBuild v vs f) = pretty v <+> equals <+> "build" <+> pred f vs
     pdope (OPCall v vs f) = pretty v <+> equals <+> "call" <+> pred f vs

     pdope (OPIter o m f) =
           let mo = m ++ [o] in
               "for" <+> (tupled $ filterBound mo) <+> "in" <+> pretty f <> slice mo


     slice = brackets . sepBy "," . map (\x -> case x of (MF v) -> ":" ; (MB v) -> pnt v)

     filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

     pred f vs = pretty f <> (tupled $ map pnt vs)

     pnt (NTNumeric (Left x))  = pretty x
     pnt (NTNumeric (Right x)) = pretty x
     pnt (NTString s)          = dquotes (pretty s)
     pnt (NTVar v)             = pretty v


writePlans file = do
    contents <- B.readFile file
    writeFile (file ++ ".plan") $ show $ prettyPlans contents
    return ()
