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

procANF :: Rule -> DFunctAr
procANF (Rule _ h _ _ sp _ crs _) =
  -- XXX findHeadFA is bad and I should feel bad
  case findHeadFA h crs of
    Nothing -> dynacSorry $ "The rule at" <+> (prettySpanLoc sp)
                                          <+> "is beyond my abilities."
    Just t  -> t

buildAggMap :: AggMap -> [Rule] -> AggMap
buildAggMap = go
 where
  go m []      = m
  go m (ar@(Rule _ _ a _ sp _ _ _):xs) =
    let d = procANF ar
    in case mapUpsert d a m of
         Left a' -> dynacUserErr $     "Conflicting aggregators; rule"
                                   <+> prettySpanLoc sp <+> "uses" <+> (pretty a)
                                   <+> "but I had been lead to expect"
                                   <+> pretty a'
         Right m' -> go m' xs

------------------------------------------------------------------------}}}
