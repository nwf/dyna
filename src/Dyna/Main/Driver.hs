---------------------------------------------------------------------------
-- | Main driver of the pipeline

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-} -- This is probably a terrible idea, but
                                -- I'd never done it before and wanted to
                                -- see what it's like.  It won't be hard to
                                -- rip out.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Main.Driver where

import           Control.Applicative ((<*))
import           Control.Exception
-- import           Control.Monad
import qualified Data.ByteString.UTF8         as BU
import           Data.Either
import qualified Data.Map                     as M
import qualified Data.IntMap                  as IM
import qualified Data.Maybe                   as MA
import qualified Data.Set                     as S
import           Data.String
import           Data.Version (showVersion)
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.ANF
import           Dyna.Analysis.ANFPretty
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.BackendDefn
import           Dyna.Backend.Backends
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.OneshotDriver  as P
import           Dyna.Term.TTerm
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.PrettyPrint.Free        as PP
import qualified Text.PrettyPrint.ANSI.Leijen as PPA
import qualified Text.Trifecta                as T
import qualified Text.Trifecta.Result         as TR

import qualified Paths_dyna                   as CPD

------------------------------------------------------------------------}}}
-- Version display                                                      {{{

version :: String
version = showVersion (CPD.version)

------------------------------------------------------------------------}}}
-- Dumping                                                              {{{

data DumpType = DumpAgg
              | DumpANF
              | DumpDopIni
              | DumpDopUpd
              | DumpFailedPlans
              | DumpParsed
 deriving (Eq,Ord,Show)

type DumpMap = M.Map DumpType (Maybe FilePath)

dump :: (?dcfg :: DynacConfig) => DumpType -> Doc e -> IO ()
dump dt doc =
  case M.lookup dt (dcfg_dumps ?dcfg) of
    Nothing       -> return ()
    Just Nothing  -> go True stderr
    Just (Just f) -> bracket (openFile f WriteMode) hClose (go False)
 where
  go h f = hPutDoc f $
    if h
     then    header `above` doc <> line
          <> hcat (replicate 8 bar) <> line
     else doc

  header = bar <+> fill 58 (text $ show dt) <+> bar
  bar    = "=========="

anyDumpStderr :: (?dcfg :: DynacConfig) => Bool
anyDumpStderr = M.foldr (\v r -> r || MA.isNothing v)
                        False (dcfg_dumps ?dcfg)

dumpOpts :: Bool -> [OptDescr Opt]
dumpOpts nos =
     mkDumpOpt "agg"    DumpAgg     "Aggregator summary"
  ++ mkDumpOpt "anf"    DumpANF     "Administrative Normal Form"
  ++ mkDumpOpt "dopini" DumpDopIni  "DOpAMine planning results: initializers"
  ++ mkDumpOpt "dopupd" DumpDopUpd  "DOpAMine planning results: updates"
  ++ mkDumpOpt "failed-plans" DumpFailedPlans "Planner failures"
  ++ mkDumpOpt "parse"  DumpParsed  "Parser output"
 where
  mkDumpOpt arg fl hm =
    Option [] ["dump-"    ++ arg] (OptArg (OptDump . sfl) "FILE") hm
    : if nos
       then [ Option [] ["no-dump-" ++ arg] (NoArg (OptDump cfl)) "" ]
       else []
   where
    sfl x fs = M.insert fl x fs
    cfl   fs = M.delete fl   fs

------------------------------------------------------------------------}}}
-- DynacConfiguration                                                   {{{

data DynacConfig = DynacConfig { dcfg_backend :: !Backend
                               , dcfg_dumps   :: !DumpMap
                               , dcfg_outFile :: !(Maybe FilePath)
                               }


defaultDynacConfig :: DynacConfig
defaultDynacConfig = DynacConfig
  { dcfg_backend = noBackend
  , dcfg_dumps   = M.empty
  , dcfg_outFile = Nothing
  }

------------------------------------------------------------------------}}}
-- Options and Argument Handling                                        {{{

data QuickExit = QEBiblio
               | QEHelp
               | QEHelpBackend
               | QEHelpDump
               | QEVersion
               | QEVersionNumber

quickExit :: QuickExit -> IO ()
quickExit QEBiblio = do
  qeBanner "Recommended readings"
  PPA.putDoc $ PPA.vcat
   [ (PPA.<$>) (for PPA.green "users")
     $ PPA.indent 2 $ PPA.vcat
       [ "Documentation, including a tutorial,"
         PPA.<+> "is available by running"
         PPA.<+> PPA.white "make sphinxdoc"
         PPA.<+> PPA.dot
       ]
   , (PPA.<$>) (for PPA.yellow "theoreticians and academics")
     $ PPA.indent 2 $ PPA.vcat
       [ "Several papers are available at"
         PPA.<+> PPA.underline
                    "http://www.dyna.org/wiki/index.php?title=Publications"
         PPA.<+> PPA.dot
       ]
   , (PPA.<$>) (for PPA.red "developers")
     $ PPA.indent 2 $ PPA.vcat
       [ "Source-based documentation can be built by"
         PPA.<+> PPA.white "make haddock"
         PPA.<+> PPA.dot
       ]
   , PPA.empty
   ]
 where
  for c w = "For" PPA.<+> c w PPA.<> PPA.colon

quickExit QEHelp = do
  qeBanner "Help! I need somebody! Help! Not just anybody! Heeelp!"
  putStrLn disclaimer
  putStrLn (usageInfo h helpfulOptions)
 where
  disclaimer =  "This version of Dyna2 represents a prototype!\n"
             ++ "There are known inefficiencies and less-than-ideal code.\n"
             ++ "We hope that you enjoy using it despite these woes! :)"

  h = "\nUsage: dyna -B backend [-o FILE.out] FILE.dyna\n\nOption summary:"
quickExit QEHelpBackend = do
  qeBanner "Backend information"
  putDoc backendHelp
  putStrLn ""
quickExit QEHelpDump = do
  qeBanner "Debugging dumps"
  putStrLn (usageInfo "" $ dumpOpts False)
quickExit QEVersion = putStrLn $ "Dyna " ++ version
quickExit QEVersionNumber = putStrLn version

qeBanner :: String -> IO ()
qeBanner s = putStrLn $ "Dyna " ++ version ++ ": " ++ s

data Opt = OptQE QuickExit
         | OptBackend Backend
         | OptDump (DumpMap -> DumpMap)
         | OptOutput  FilePath

helpOpt :: OptDescr Opt
helpOpt = Option ['h'] ["help"]    (NoArg $ OptQE QEHelp)    "display this help message"

helpMoreOpts :: [OptDescr Opt]
helpMoreOpts =
  [ Option [] ["help-dump"] (NoArg $ OptQE QEHelpDump) "show --dump-* options"
  , Option [] ["help-backend"] (NoArg $ OptQE QEHelpBackend) "show backend information"
  ]

infoOpts :: [OptDescr Opt]
infoOpts = 
  [ Option ['V'] ["version"] (NoArg $ OptQE QEVersion) "display version and exit"

  -- This is an excellent idea, taken from 'pi' at http://www.ginac.de/CLN/
  , Option [] ["bibliography"] (NoArg $ OptQE QEBiblio) "relevant papers"
  ]

-- | Miscellaneous options which the user probably does not care about
unhelpfulOpts :: [OptDescr Opt]
unhelpfulOpts =
  [ Option [] ["version-number"] (NoArg $ OptQE QEVersionNumber)
                                 "Display just the version number and exit"
  ]

coreOpts :: [OptDescr Opt]
coreOpts =
  [ Option ['B'] ["backend"]      (ReqArg obe "BE")
    "use backend BE"
  , Option ['o'] ["out","output"] (ReqArg OptOutput "FILE")
    "write generated output to FILE"
  ]
 where
  obe = OptBackend . parseBackend


allOptions :: [OptDescr Opt]
allOptions =
  helpOpt : helpMoreOpts ++ infoOpts ++ coreOpts ++ (dumpOpts True)
          ++ unhelpfulOpts

-- | When the user has asked for help, what do they want to see?
helpfulOptions :: [OptDescr Opt]
helpfulOptions = helpMoreOpts ++ infoOpts ++ coreOpts

procArgs :: [String] -> IO (DynacConfig, [String])
procArgs argv = do
  case getOpt Permute allOptions argv of
    (os,as,[]) -> case foldOpts os of
                    Left x -> do
                               quickExit x
                               exitSuccess
                    Right f -> return $ (f defaultDynacConfig, as)
    (_,_,es)   -> dynacThrow $ InvocationError $ "Arguments not understood: " <> vcat (map pretty es)
 where
  foldOpts :: [Opt] -> Either QuickExit (DynacConfig -> DynacConfig)
  foldOpts = go id
   where
    go f []                = Right f
    go _ (OptQE x:_)       = Left x
    go f (OptBackend b:os) = go (setBackend b  . f) os
    go f (OptDump f':os)   = go (mungeDump  f' . f) os
    go f (OptOutput fn:os) = go (setOutput fn  . f) os

    setBackend b c = c { dcfg_backend = b }
    mungeDump  f c = c { dcfg_dumps = f $ dcfg_dumps c }
    setOutput  o c = c { dcfg_outFile = if o == "-" then Nothing else Just o }

------------------------------------------------------------------------}}}
-- Showing DOpAMine                                                     {{{

renderDop :: BackendRenderDopIter bs e -> Actions bs -> Doc e
renderDop ddi dop = vsep $ map (renderDOpAMine ddi) dop

renderDopUpds :: BackendRenderDopIter bs e
              -> [(Rule,[(Int,Maybe DFunctAr,Cost,DVar,DVar,Actions bs)])]
              -> Doc e
renderDopUpds ddi us = vsep $ flip map us $ \(r,ps) ->
    vsep $ flip map ps $ \(n,fa,c,vi,vo,act) ->
        planHeader r fa n c (vi,vo) `above` indent 2 (renderDop ddi act)
 where
  planHeader r (fa :: Maybe DFunctAr) n c (vi,vo) =
        text ";;"
    <+> (prettySpanLoc $ r_span r)
    <+> text "ruleix=" <> pretty (r_index r)
    <+> text "fa="     <> maybe empty
                                (\(f,a) -> pretty f <> text "/" <> pretty a)
                                fa
    <+> text "evalix=" <> pretty n
    <+> text "cost="   <> pretty c
    <+> text "in="     <> pretty vi
    <+> text "out="    <> pretty vo

renderDopInis :: BackendRenderDopIter bs e
              -> [(Rule,Cost,Actions bs)]
              -> Doc e
renderDopInis ddi im = vsep $ flip map im $ \(r,c,ps) ->
  iniHeader r c `above` indent 2 (renderDop ddi ps)
 where
  iniHeader r c = 
        text ";;"
    <+> (prettySpanLoc $ r_span r)
    <+> text "ruleix=" <> pretty (r_index r)
    <+> text "cost=" <> pretty c
    <+> text "head=" <> pretty (r_head r)
    <+> text "res=" <> pretty (r_result r)

renderFailedInit rd (r,ps) =
       text ";; failed initialization attempts for"
  <//> (prettySpanLoc $ r_span r)
   <+> text "ruleix=" <> pretty (r_index r)
  <//> indent 2 (vsep $ map (renderPartialPlan rd) ps)

renderFailedUpdate rd (r,i,ps) =
       text ";; failed update plans for"
  <//> (prettySpanLoc $ r_span r)
   <+> text "ruleix=" <> pretty (r_index r)
   <+> (text "evalix=" <> pretty i)
  <//> indent 2 (vsep $ map (renderPartialPlan rd) ps)

renderFailedQuery rd (r,pbce) =
       text ";; failed query attempts for"
  <//> (prettySpanLoc $ r_span r)
   <+> text "ruleix=" <> pretty (r_index r)
  <//>
  case pbce of
    PBCWrongFunctor f -> "wrong functor" <+> squotes (pretty f)
    PBCWrongArity n   -> "wrong arity"   <+> squotes (pretty n)
    PBCBadRule        -> "bad rule"
    PBCNoPlan ps      -> "no plan:"
                         <//> indent 2 (vsep $ map (renderPartialPlan rd) ps)

------------------------------------------------------------------------}}}
-- Warnings                                                             {{{

renderSpannedWarn :: BU.ByteString -> [T.Span] -> Doc e
renderSpannedWarn w s = "WARNING:" <+> text (BU.toString w) <+> "AT"
                        `above` indent 2 (vcat (map prettySpanLoc s))

------------------------------------------------------------------------}}}
-- Pipeline!                                                            {{{

processFile :: (?dcfg :: DynacConfig) => FilePath -> IO ()
processFile fileName = bracket openOut hClose go
 where
  openOut = maybe (return stdout) (flip openFile WriteMode)
            $ dcfg_outFile ?dcfg

  maybeWarnANF [] = Nothing
  maybeWarnANF xs = Just $ vcat $ map (uncurry renderSpannedWarn) xs

  go out = do
    P.PDP rs iaggmap gbcs pp <- parse (be_aggregators $ dcfg_backend ?dcfg)

    dump DumpParsed $
         (vcat $ map (\(i,_,r) -> text $ show (i,r)) rs)
     <> line <> pp
   
    let (frs, anfWarns) = unzip $ map normRule rs

    dump DumpANF (vcat $ map renderANF frs)

    hPutDoc stderr $ vcat $ MA.mapMaybe maybeWarnANF anfWarns

    aggm <- return $! buildAggMap iaggmap frs

    dump DumpAgg (M.foldlWithKey (\d f a -> d `above`
                                    (pretty f <+> colon <+> pretty a))
                                 empty aggm)


    case dcfg_backend ?dcfg of
      Backend _ be_b be_c be_ddi be_d ->
        let
            (bcrules,fcrules) = partitionEithers
                                $ map
                                  (\r -> maybe (p r)
                                               (\fa -> if fa `S.member` gbcs
                                                        then Left (fa,r)
                                                        else Right r)
                                        $ findHeadFA (r_head r) (r_ucruxes r))
                                  frs
              where
               p r = (dynacPanic $ "Can't check rule"
                                 <+> (prettySpanLoc (r_span r))
                                 <+> "for chaining direction")

            initializers = map (\(f,mca) -> either (\e -> Left (f,e))
                                                   (\(c,a) -> Right (f,c,a)) mca)
                         $ map (\x -> (x, planInitializer be_b gbcs x)) fcrules

            cInitializers = rights initializers
  
            uPlans = map (\x -> (x, planEachEval be_b gbcs be_c x)) fcrules

            qPlans = map (\(fa@(f,a),r) -> (fa,r,
                                            planBackchain be_b gbcs (f,mkqm a) r))
                         bcrules
                      where
                       mkqm a = QMode (replicate a (tus,tus)) (tf,tus) DetSemi

                       tus = nHide $ IUniv UShared
                       tf  = nHide IFree

            cqPlans = map check qPlans
                       where
                        check (f,r,Right p) = (f,r,p)
                        check (_,r,Left  _) = dynacUserErr $
                          "Unable to plan backchaining for rule"
                          <//> (prettySpanLoc (r_span r))
                          <> dot

        in do
            -- Do this before forcing cInitializers, uPlans, etc.,
            -- as those will panic and stop the pipeline.
            dump DumpFailedPlans $
              let rend = renderDop be_ddi
              in vcat [ vcat $ map (renderFailedInit rend)
                             $ lefts initializers
                      , let
                          shuffle (r,ips) = map sgo ips
                           where
                            sgo (i,Left e) = Left (r,i,e)
                            sgo (_,Right _) = Right ()
                        in vcat $ map (renderFailedUpdate rend)
                                $ lefts
                                $ concat
                                $ map shuffle uPlans
                      , let
                          shuffle (_,r,Right _) = Nothing
                          shuffle (_,r,Left  e) = Just (r,e)
                        in vcat $ map (renderFailedQuery rend)
                                $ MA.catMaybes $ map shuffle qPlans
                      ]

            -- Force evaluation of a lot of the work of the compiler,
            -- even if the backend and dump flags won't do it for us.
            cInitializers' <- mapM evaluate cInitializers
            uPlans' <- evaluate (forceUpdatePlans uPlans)

            let noInitErrGbcs = filter (\(r,_) ->
                    maybe True
                          (\fa -> not $ fa `S.member` gbcs)
                        $ findHeadFA (r_head r) (r_ucruxes r))

            case noInitErrGbcs $ lefts initializers of
              [] -> return ()
              xs -> dynacUserErr $ "Unable to plan initializers for rule(s):"
                               <//> (indent 2 $ vcat $
                                     map (prettySpanLoc . r_span . fst) xs)

            dump DumpDopIni (renderDopInis be_ddi cInitializers')
            dump DumpDopUpd (renderDopUpds be_ddi uPlans')

            -- Invoke the backend code generator
            be_d aggm uPlans' cInitializers' gbcs cqPlans pp out

  parse aggs = do
    pr <- T.parseFromFileEx (P.oneshotDynaParser aggs <* T.eof) fileName
    case pr of
      TR.Failure td -> dynacParseErr $ PPA.align td
      TR.Success rs -> return rs

  forceUpdatePlans :: [(Rule,[(Int,
                               Either a (Cost, DVar, DVar, Actions fbs))])]
                   -> [(Rule,[(Int,
                               Maybe DFunctAr, Cost, DVar, DVar, Actions fbs)])]

  forceUpdatePlans = map goRule
   where
    goRule (r,l) = let mps = map (goEvalix r) l in mps `seq` (r,mps)

    goEvalix r (i,efp) = fa `seq` c `seq` (i,fa,c,ii,iv,p)
     where
      (c,ii,iv,p) = case efp of
        Left _ -> dynacUserErr
                         $ "No update plan for"
                            <+> maybe "indirection"
                                      (\(f,a) -> pretty f <> char '/' <> pretty a)
                                      fa
                            <+> "in rule at"
                            <> line <> indent 2 (prettySpanLoc $ r_span r)
        Right x -> x

      fa = evalCruxFA ev
      ev = maybe (dynacPanic $ "Eval index without eval crux in rule:"
                               <//> (renderANF r))
                 id
                 (IM.lookup i (r_ecruxes r))

------------------------------------------------------------------------}}}
-- Main                                                                 {{{

main_ :: [String] -> IO ()
main_ argv = do
  (dcfg, fis) <- procArgs argv
  let ?dcfg = dcfg
  case fis of
    []  -> dynacThrow $ InvocationError "Must specify a Dyna file"
    [x] -> processFile x
    _   -> dynacSorry "We can't do more than one file"

main :: IO ()
main = catches (getArgs >>= main_)
               [Handler printerr, Handler exit, Handler someExnPanic]

 where
  printerr x = pe x >> exitFailure

  pe (UserProgramError d) = do
    PP.hPutDoc stderr (upeMsg <> line <> PP.indent 1 d <//> upePostfix)
    hPutStrLn stderr ""
  pe (UserProgramParseError d) = do
    PPA.hPutDoc stderr (parseMsg <> PPA.line <> PPA.indent 1 d)
    hPutStrLn stderr ""
  pe (InvocationError d) = do
    PP.hPutDoc stderr ("Invocation error:" <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
    quickExit QEHelp
  pe (Sorry d) = do
    PP.hPutDoc stderr (sorryMsg <> line <> taMsg <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
  pe (Panic d) = panic d

  exit ExitSuccess = return ()
  exit (ExitFailure n) = panic $ "Haskell exit code: " <+> pretty n

  someExnPanic (e :: SomeException) = panic $ "Uncaught Haskell exception:"
                                              <+> text (show e)

  panic d = do
    PP.hPutDoc stderr (panicMsg <> line <> taMsg <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
    exitFailure

  upeMsg :: (IsString s) => s
  upeMsg = "Encountered error in input program:"

  upePostfix :: Doc e
  upePostfix =      "Everything was syntactically valid, but we could not"
               <//> "see it through."

  parseMsg = "Could not parse:"

  sorryMsg :: (IsString s) => s
  sorryMsg = "Terribly sorry, but you've hit an unsupported feature"

  panicMsg :: (IsString s) => s
  panicMsg = "Compiler panic!"

  taMsg :: (IsString s) => s
  taMsg = "This is almost assuredly not your fault!  Please contact a TA."
------------------------------------------------------------------------}}}
