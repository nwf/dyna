---------------------------------------------------------------------------
-- | Check that the aggregations in a program are well-founded.
--
-- Consumes "Dyna.Analysis.ANF" for a whole program.


-- Header material                                                      {{{
module Dyna.Analysis.Aggregation (
    AggMap, buildAggMap
) where

import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import           Dyna.Analysis.ANF
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

type AggMap = M.Map DFunctAr DAgg

------------------------------------------------------------------------}}}
-- Processing                                                           {{{

-- XXX These functions really would like to have span information, so they
-- could report which line of the source caused an error.

procANF :: (FDR, ANFState) -> Either String (DFunctAr, DAgg)
procANF (FRule h a _ _, AS { as_unifs = us }) =
  case M.lookup h us of
    Nothing       -> Left $ "I can't process head-variables"
    Just t -> case t of
                TString _     -> Left $ "Malformed rule with string head"
                TNumeric _    -> Left $ "Malformed rule with numeric head"
                TFunctor f as -> Right ((f,length as),a)

buildAggMap :: [(FDR, ANFState)] -> Either String AggMap
buildAggMap = go (M.empty)
 where
  go m []      = Right m
  go m (ar:xs) =
    case procANF ar of
      Left e -> Left e 
      Right (d,a) ->
        case mapUpsert d a m of
          Left a' -> Left $ "Conflicting aggregators"
          Right m' -> go m' xs

------------------------------------------------------------------------}}}
