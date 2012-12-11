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
import qualified Data.ByteString.UTF8       as BU
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
   <+> functorIndirect "peel" f vs <> (parens $ pretty id)

pdope (OPBuild v vs f) = pretty v <+> equals
      <+> functorIndirect "build" f vs
      <> (tupled $ map pretty vs)

pdope (OPCall v vs f) = pretty v <+> equals
      <+> functorIndirect "call" f vs
      <> (tupled $ map pretty vs)

pdope (OPIter o m f) =
      let mo = m ++ [o] in
          "for" <+> (tupled $ filterBound mo)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

pslice = brackets . sepBy ","
         . map (\x -> case x of (MF v) -> ":" ; (MB v) -> pretty v)

filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

functorIndirect table f vs = table <> (brackets $ dquotes $ (pretty f <> "/" <> (text $ show $ length vs)))

pf f vs = pretty f <> (tupled $ map pretty vs)

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

-- XXX This belongs elsewhere.
--
-- XXX This guy wants span information.
--
-- timv: might want to fuse these into one circuit
--
combinePlans :: [(FRule,[(DFunctAr, Maybe (Cost,Action))])] ->
                Either String (M.Map DFunctAr [(FRule, Cost, Action)])    -- all plans for functor/arity
                                                                          -- XXX: are FDR's unique keys? suppose a rule is repeated?
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


-- timv: consider flattening FRUle and ANFState

py (f,a) mu (FRule h _ _ r span _) dope =
           case mu of
             Just (hv,v) ->
                         "@register"
                 <>      pfsa
                 `above` "def" <+> char '_'
                               <+> tupled (map pretty [hv,v])
                               <+> colon
             Nothing -> "@initializer" <> pfsa
                 `above` "def _():"
   `above` (indent 4 $ go dope emit)

 where
   pfsa = (parens $ dquotes $
            pretty f <> "/" <> (text $ show a))

   go []  = id
   go (x:xs) = let px = pdope x
                   indents = case x of OPIter _ _ _ -> True ; _ -> False
               in
                   above px . (if indents then indent 4 else id) . go xs

   emit = "emit" <> tupled [pretty h, pretty r]


printPlan :: Handle
          -> (DFunct,Int)                    -- ^ Functor & arity
          -> Maybe (DVar,DVar)               -- ^ if update, input intern & value
          -> (FRule, Cost, Action)           -- ^ rule and plan
          -> IO ()
printPlan fh fa mu (r, cost, dope) = do         -- display plan
  hPutStrLn fh $ "# --"
  displayIO fh $ prefixSD "# " $ renderPretty 1.0 100
                 $ (renderSpan $ fr_span r) <> line
  hPutStrLn fh $ "# Cost: " ++ (show cost)
  displayIO fh $ renderPretty 1.0 100
                 $ py fa mu r dope <> line
  hPutStrLn fh ""
 where
  renderSpan (T.Span s e bs) =
         T.prettyTerm s
     <+> char '-'
     <+> T.prettyTerm e
     <+> colon
     `above` (indent 2 (T.prettyTerm $ T.rendering s bs))



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
      let urs = map (\(P.LRule x T.:~ _) -> x) rs
          frs = map normRule urs
          initializers = MA.mapMaybe (\(f,mca) -> (\(c,a) -> (f,c,a)) `fmap` mca)
                         $ map (\x -> (x, planInitializer x)) frs
      in do
         aggm <- case buildAggMap frs of
                   Left e -> throw $ TLEAggPlan e     -- multiple aggregators
                   Right a -> return a
         cPlans <- case combinePlans                  -- crux plans
                      $ map (\x -> (x, planEachEval headVar valVar x)) frs of
                    Left e -> throw $ TLEUpdPlan e    -- no plan found
                    Right a -> return a
         forM_ (M.toList cPlans) $ \(fa, ps) -> do    -- plans aggregated by functor/arity
            hPutStrLn fh ""
            hPutStrLn fh $ "# =============="
            hPutStrLn fh $ "# " ++ show fa
            forM_ ps $ printPlan fh fa (Just (headVar,valVar))
         hPutStrLn fh ""
         hPutStrLn fh $ "# =============="
         hPutStrLn fh $ "# Initializers"
         forM_ initializers $ \(f,c,a) -> printPlan fh (findHeadFA f) Nothing (f,c,a)

 where
  findHeadFA (FRule h _ _ _ _ (AS { as_unifs = us })) =
    case M.lookup h us of
      Nothing            -> error "No unification for head variable?"
      Just (Left _)      -> error "NTVar head?"
      Just (Right (f,a)) -> (f, length a)

  headVar = "_h"
  valVar  = "_v"


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