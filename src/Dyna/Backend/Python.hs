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

import Control.Applicative ((<*))

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

pdope :: DOpAMine -> Doc e
pdope (OPIndirEval _ _) = error "indirect evaluation not implemented"
pdope (OPAssign v val) = pretty v <+> equals <+> pretty val
pdope (OPCheck v val) = hsep ["assert", pretty v, "==", pretty val]
pdope (OPGetArgsIf vs id f) =
       tupled (map pretty vs)
   <+> equals
   <+> "peel" <> (parens $ fa f vs <> comma <> pretty id)

pdope (OPBuild v vs f) = pretty v <+> equals
      <+> "build" <> (parens $ fa f vs <> comma <> (sepBy "," $ map pretty vs))

pdope (OPCall v vs f) = pretty v <+> equals
      <+> functorIndirect "call" f vs
      <> (tupled $ map pretty vs)

pdope (OPIter o m f) =
      let mo = m ++ [o] in
          "for" <+> (tupled $ filterBound mo)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

fa f a = dquotes $ pretty f <> "/" <> (text $ show $ length a)

pslice = brackets . sepBy ","
         . map (\x -> case x of (MF v) -> ":" ; (MB v) -> pretty v)

filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

functorIndirect table f vs = table <> (brackets $ dquotes $ (pretty f <> "/" <> (text $ show $ length vs)))

pf f vs = pretty f <> (tupled $ map pretty vs)

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

-- XXX This belongs elsewhere.
--
-- XXX This guy wants span information; he's got it now use it.
--
-- timv: might want to fuse these into one circuit
--
combinePlans :: [(FRule,[(DFunctAr, Maybe (Cost,Action))])] ->
                Either String (M.Map DFunctAr [(FRule, Cost, Action)])    -- all plans for functor/arity
combinePlans = go (M.empty)
 where
  go m []             = Right m
  go m ((fr,cmca):xs) = go' xs fr cmca m

  go' xs _  []           m = go m xs
  go' xs fr ((fa,mca):ys) m =
    case mca of
      Nothing -> Left $ "No plan for " ++ (show fa)    -- timv: throw error here?
                            ++ " in " ++ (show fr)
      Just (c,a) -> go' xs fr ys $ iora fa (fr,c,a) m

  -- Insert OR Append
  iora :: (Ord k) => k -> v -> M.Map k [v] -> M.Map k [v]
  iora k v m = M.alter (\mv -> Just $ v:nel mv) k m
   where
    nel Nothing  = []
    nel (Just x) = x


py (cruxf,cruxa) (FRule h _ _ r span _) dope =
           "@register" <> (parens $ dquotes $ pretty cruxf <> "/" <> (text $ show cruxa))
   `above` "def _(_H, _V):"
   `above` (indent 4 $ go dope)

 where
   go [x] = pdope x `above` emit
   go (x:xs) = let px = pdope x
                   pxstr = (show $ px)
                   indents = ((pxstr !! (length pxstr - 1)) == ':')
               in
                   px `above` (if indents then indent 4 $ go xs else go xs)

   emit = "emit" <> tupled [pretty h, pretty r]



processFile fileName = do
  fh <- openFile (fileName ++ ".plan") WriteMode
  processFile_ fileName fh
  hClose fh


processFileStdout fileName = do
  processFile_ fileName stdout


processFile_ fileName fh = do
  pr <- T.parseFromFileEx (P.dlines <* T.eof) fileName

  case pr of
    T.Failure td -> T.display td
    T.Success rs ->
      let urs  = map (\(P.LRule x T.:~ _) -> x) rs
          anfs = map normRule urs
      in do
         aggm <- case buildAggMap anfs of             -- only used for error checking?
                   Left e -> throw $ TLEAggPlan e     -- multiple aggregators
                   Right a -> return a
         cPlans <- case combinePlans                  -- crux plans
                      $ map (\x -> (x, planEachEval headVar valVar x)) anfs of
                    Left e -> throw $ TLEUpdPlan e    -- no plan found
                    Right a -> return a
         forM_ (M.toList cPlans) $ \(fa, ps) -> do    -- plans aggregated by functor/arity
            hPutStrLn fh $ "\n# =============="
            hPutStrLn fh $ "# " ++ show fa
            forM_ ps $ \(r, cost, dope) -> do         -- display plan
                hPutStrLn fh $ "# --"
                hPutStrLn fh $ "# Cost: " ++ (show cost)
                displayIO fh $ renderPretty 1.0 100
                          $ py fa r dope
                hPutStrLn fh ""

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
                                printANF $ normRule x)
                                (unsafeParse P.dlines contents))
                      <> line)
    return ()
------------------------------------------------------------------------}}}
