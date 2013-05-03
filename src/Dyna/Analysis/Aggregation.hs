---------------------------------------------------------------------------
-- | Check that the aggregations in a program are well-founded.
--
-- Consumes "Dyna.Analysis.ANF" for a whole program.


-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Analysis.Aggregation (
    AggMap, buildAggMap
) where

-- import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import           Dyna.Analysis.ANF
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Dyna.XXX.Trifecta
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

type AggMap = M.Map DFunctAr DAgg

------------------------------------------------------------------------}}}
-- Associate each item with an aggregator                               {{{

procANF :: Rule -> (DFunctAr, DAgg)
procANF r@(Rule _ h a _ sp (AS { as_assgn = as })) =
  case M.lookup h as of
    Nothing       -> dynacSorry $ "I can't process head-variables in rule at" <+> (prettySpanLoc sp)
    Just t -> case t of
                Left _       -> dynacPanic $ "Malformed head" <+> (pretty $ show r)
                Right (f,xs) -> ((f,length xs),a)

buildAggMap :: [Rule] -> AggMap
buildAggMap = go (M.empty)
 where
  go m []      = m
  go m (ar@(Rule _ _ a _ sp _):xs) =
    let (d,a) = procANF ar
    in case mapUpsert d a m of
         Left a' -> dynacUserErr $     "Conflicting aggregators; rule"
                                   <+> prettySpanLoc sp <+> "uses" <+> (pretty a)
                                   <+> "but I had been lead to expect"
                                   <+> pretty a'
         Right m' -> go m' xs

------------------------------------------------------------------------}}}
