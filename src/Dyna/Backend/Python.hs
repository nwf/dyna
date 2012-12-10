---------------------------------------------------------------------------
-- | Some week-before-the-deadline heroics to try to get something
-- (anything) up and running.
--
-- XXX This is terrible.  Just terrible.

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Backend.Python where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.Char
-- import           Data.Either
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Ord                   as O
import qualified Data.Set                   as S
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.RuleMode
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.XXX.PPrint
import           Dyna.XXX.TrifectaTest
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

processFile fileName = do
  pr <- T.parseFromFileEx (P.dlines) fileName
  case pr of
    T.Failure td -> T.display td
    T.Success rs ->
      let urs  = map (\(P.LRule x T.:~ _) -> x) rs
          anfs = map (runNormalize . normRule) urs
          eaggm = buildAggMap anfs
      in -- Ensure that we have an aggregator plan
         case eaggm of
           Left e -> print e >> putStrLn "while building aggregator map."
           Right aggm -> print "Got an agg plan..."
             -- XXX now, build an update plan for each rule
             

------------------------------------------------------------------------}}}
-- Experimental Residuals?                                              {{{

-- | Normalize all the rules in a file and emit S-exprs for the ANF
-- normalized form.
--
-- NOTE: This is used by bin/prototype.py
normalizeFile file = do
    contents <- B.readFile file
    writeFile (file ++ ".anf")
              (show $ vcat (map (\(P.LRule x T.:~ _) ->
                                printANF $ runNormalize $ normRule x)
                                (unsafeParse P.dlines contents))
                      <> line)
    return ()
------------------------------------------------------------------------}}}
