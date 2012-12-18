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
-- Associate each item with an aggregator                               {{{

-- XXX These functions should be rewritten to use Dyna.Main.Exception

-- XXX These functions really would like to have span information, so they
-- could report which line of the source caused an error.

procANF :: Rule -> Either String (DFunctAr, DAgg)
procANF (Rule _ h a _ _ _ (AS { as_assgn = as })) =
  case M.lookup h as of
    Nothing       -> Left $ "I can't process head-variables"
    Just t -> case t of
                Left _       -> Left "Malformed head"
                Right (f,as) -> Right ((f,length as),a)

buildAggMap :: [Rule] -> Either String AggMap
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
