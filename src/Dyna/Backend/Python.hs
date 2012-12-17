---------------------------------------------------------------------------
-- | Compile to Python
--
-- See bin/stdlib.py

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Backend.Python where

import           Control.Applicative ((<*))
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
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.RuleMode
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.XXX.DataUtils (mapInOrApp)
import           Dyna.XXX.PPrint
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T



------------------------------------------------------------------------}}}
-- DOpAMine Printout                                                    {{{

pdope :: DOpAMine -> Doc e
pdope (OPIndirEval _ _) = error "indirect evaluation not implemented"
pdope (OPAssign v val) = pretty v <+> equals <+> pretty val
pdope (OPCheck v val) = "if" <+> pretty v <+> "!=" <+> pretty val <> ": continue"
pdope (OPCheckNE v val) = "if" <+> pretty v <+> "==" <+> pretty val <> ": continue"
pdope (OPGetArgsIf vs id f) =

    "try:" `above` (indent 4 $
           tupledOrUnderscore vs
            <+> equals <> " "
                <> "peel" <> (parens $ fa f vs <> comma <> pretty id)
     )

    `above` "except (TypeError, AssertionError): continue"   -- you'll get a "TypeError: 'NoneType' is not iterable."


pdope (OPBuild v vs f) = pretty v <+> equals
      <+> "build" <> (parens $ fa f vs <> comma <> (sepBy "," $ map pretty vs))

pdope (OPCall v vs f) = pretty v <+> equals
      <+> functorIndirect "call" f vs
      <> (tupled $ map pretty vs)

pdope (OPIter o m f) =
      let mo = m ++ [o] in
          "for" <+> (tupledOrUnderscore $ filterBound mo)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

fa f a = dquotes $ pretty f <> "/" <> (text $ show $ length a)

-- this comes up because can't assign to ()
tupledOrUnderscore vs = if length vs > 0 then parens ((sepBy "," $ map pretty vs) <> ",") else text "_"

pslice vs = brackets $
       sepBy "," (map (\x -> case x of (MF v) -> ":" ; (MB v) -> pretty v) vs)
       <> "," -- add a list comma to ensure getitem is always passed a tuple.

filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

functorIndirect table f vs = table <> (brackets $ dquotes $ (pretty f <> "/" <> (text $ show $ length vs)))

pf f vs = pretty f <> (tupled $ map pretty vs)

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

-- | Return all plans for each functor/arity
--
-- XXX This belongs elsewhere.
--
-- XXX This guy wants span information; he's got it now use it.
--
-- timv: might want to fuse these into one circuit
--
combinePlans :: [(FRule,[(DFunctAr, Maybe (Cost,Action))])] ->
                M.Map DFunctAr [(FRule, Cost, Action)]
combinePlans = go (M.empty)
 where
  go m []             = m
  go m ((fr,cmca):xs) = go' xs fr cmca m

  go' xs _  []           m = go m xs
  go' xs fr ((fa,mca):ys) m =
    case mca of
      Nothing -> throw $ UserProgramError 
                       $ "No update plan for "
                          <+> (pretty fa)
                          <+> "in rule at"
                          <+> (prettySpanLoc $ fr_span fr)
      Just (c,a) -> go' xs fr ys $ mapInOrApp fa (fr,c,a) m

py (f,a) mu (FRule h _ _ r span _) dope =
           case mu of
             Just (hv,v) ->
                         "@register"
                 <>      pfsa
                 `above` "def" <+> char '_'
                               <> tupled (map pretty [hv,v])
                               <+> colon
             Nothing -> "@initializer" <> pfsa
                 `above` "def _():"
   `above` (indent 4 $ "for _ in [None]:")
   `above` (indent 8 $ go dope emit)

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
                 $ (prettySpanLoc $ fr_span r) <> line
  hPutStrLn fh $ "# Cost: " ++ (show cost)
  displayIO fh $ renderPretty 1.0 100
                 $ py fa mu r dope <> line
  hPutStrLn fh ""

processFile fileName = bracket
  (openFile (fileName ++ ".plan") WriteMode)
  hClose
  $ processFile_ fileName

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
                   Left e -> throw $ UserProgramError (text e)
                   Right x -> return x

         hPutStrLn fh $ "agg_decl = {}"
         forM (M.toList aggm) $ \((f,a),v) -> do {
             hPutStrLn fh $ show $ "agg_decl" <> brackets (dquotes $ pretty f <> "/" <> pretty a)
                <+> equals <+> (dquotes $ pretty v)
           }

         cPlans <- return $! combinePlans                  -- crux plans
                      $ map (\x -> (x, planEachEval headVar valVar x)) frs
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
  findHeadFA (FRule h _ _ _ _ (AS { as_assgn = as })) =
    case M.lookup h as of
      Nothing            -> error "No unification for head variable?"
      Just (Left _)      -> error "NTVar head?"
      Just (Right (f,a)) -> (f, length a)

  headVar = "_h"
  valVar  = "_v"


-- TEST: processFileStdout "examples/cky.dyna"

------------------------------------------------------------------------}}}
