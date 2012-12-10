---------------------------------------------------------------------------
-- | Some week-before-the-deadline heroics to try to get something
-- (anything) up and running.
--
-- XXX This is terrible.  Just terrible.

-- Header material                                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Backend.Python where

import qualified Control.Arrow              as A
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.Char
-- import           Data.Either
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Ord                   as O
import qualified Data.Set                   as S
import qualified Data.Typeable              as DT
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.RuleMode
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.XXX.PPrint
import           Dyna.XXX.TrifectaTest
import           System.IO
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- Top Level Exceptions                                                 {{{
--
-- Make the control flow a little cleaner by bailing out rather than
-- anything right-branching.  Probably not what we actually want.

data TopLevelException = TLEAggPlan String
                       | TLEUpdPlan String
 deriving (DT.Typeable,Eq,Show)

instance Exception TopLevelException

------------------------------------------------------------------------}}}
-- DOpAMine Printout                                                    {{{

-- XXX This is ripped out of Dyna.Analysis.RuleModeTest and ported over.
-- Sorry, Tim.

pdope :: DOpAMine -> Doc e
pdope (OPIndirEval _ _) = error "indirect evaluation not implemented"
pdope (OPAssign v val) = pretty v <+> equals <+> pretty val
pdope (OPCheck v val) = hsep ["assert", pretty v, "==", pretty val]
pdope (OPGetArgsIf vs id f a) =
       tupled (map pretty vs)
   <+> equals
   <+> "peel" <> parens (pretty f <> "/" <> pretty a)
              <> (parens $ pretty id)
pdope (OPBuild v vs f) = pretty v <+> equals <+> "build" <+> pf f vs
pdope (OPCall v vs f) = pretty v <+> equals <+> "call" <+> pf f vs
pdope (OPIter o m f) =
      let mo = m ++ [o] in
          "for" <+> (tupled $ filterBound mo)
                <+> "in" <+> pretty f <> pslice mo

pslice = brackets . sepBy ","
         . map (\x -> case x of (MF v) -> ":" ; (MB v) -> pretty v)

filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

pf f vs = pretty f <> (tupled $ map pretty vs)

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

-- XXX This belongs elsewhere.
--
-- XXX This guy wants span information.
combinePlans :: [(FDR,[(DFunctAr, Maybe (Cost,Action))])] ->
                Either String (M.Map DFunctAr [(Cost,Action)])
combinePlans = go (M.empty)
 where
  go m []             = Right m
  go m ((fr,cmca):xs) = go' xs fr cmca m

  go' xs _  []           m = go m xs
  go' xs fr ((fa,mca):ys) m =
    case mca of
      Nothing -> Left $ "No plan for " ++ (show fa)
                            ++ " in " ++ (show fr)
      Just ca -> go' xs fr ys $ iora fa ca m

  -- Insert OR Append
  iora :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
  iora k v m = M.alter (\mv -> Just $ v:nel mv) k m
   where
    nel Nothing  = []
    nel (Just x) = x

processFile fileName = do
  pr <- T.parseFromFileEx (P.dlines) fileName
  case pr of
    T.Failure td -> T.display td
    T.Success rs ->
      let urs  = map (\(P.LRule x T.:~ _) -> x) rs
          franfs = map (runNormalize . normRule) urs
      in do
         aggm <- case buildAggMap franfs of
                   Left e -> throw $ TLEAggPlan e
                   Right a -> return a
         cPlans <- case combinePlans
                      $ map (A.second $ planEachEval headVar valVar)
                            franfs of
                    Left e -> throw $ TLEUpdPlan e
                    Right a -> return a
         forM_ (M.toList cPlans) $ \(c,ps) -> do
            print c
            forM_ ps $ \(c,p) -> do
                putStrLn $ "# Cost: " ++ (show c)
                displayIO stdout $ renderPretty 1.0 100 $ vsep $ map pdope p
                putStrLn ""
                putStrLn ";"
            putStrLn ""
 where
  headVar = "_H"
  valVar  = "_V"

-- TEST: processFile "examples/cky.dyna"

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
