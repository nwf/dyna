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
import qualified Data.Map                     as M
import qualified Data.Maybe                   as MA
import qualified Data.Set                     as S
import           Data.String
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.ANF
import           Dyna.Analysis.ANFPretty
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.BackendDefn
import           Dyna.Backend.Backends
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Parser         as P
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

------------------------------------------------------------------------}}}
-- Dumping                                                              {{{

data DumpType = DumpAgg
              | DumpANF
              | DumpDopIni
              | DumpDopUpd
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

data DynacConfig = DynacConfig { dcfg_backend :: Backend
                               , dcfg_dumps   :: DumpMap
                               , dcfg_outFile :: Maybe FilePath
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

quickExit :: QuickExit -> IO ()
-- XXX
quickExit QEBiblio = putStrLn "Bibliographic suggestions would appear here"
quickExit QEHelp =
  putStrLn (usageInfo h helpfulOptions)
 where
  h = "\nUsage: dyna -B backend -o FILE.out FILE.dyna\n\nOption summary:"
quickExit QEHelpBackend = do
  putDoc backendHelp
  putStrLn ""
quickExit QEHelpDump = putStrLn (usageInfo "\nDump options:" $ dumpOpts False)
quickExit QEVersion = return ()


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
  -- This is an excellent idea we might consider, taken from the 'pi'
  -- program of http://www.ginac.de/CLN/
  , Option [] ["bibliography"] (NoArg $ OptQE QEBiblio) "relevant papers"
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

-- When the user has asked for help, what do they want to see?
helpfulOptions :: [OptDescr Opt]
helpfulOptions = helpMoreOpts ++ infoOpts ++ coreOpts

procArgs :: [String] -> IO (DynacConfig, [String])
procArgs argv = do
  case getOpt Permute allOptions argv of
    (os,as,[]) -> case foldOpts os of
                    Left x -> do
                               putStrLn "Dyna 0.4"
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

renderDopUpds :: BackendRenderDopIter bs e -> UpdateEvalMap bs -> Doc e
renderDopUpds ddi um = vsep $ flip map (M.toList um) $ \(fa,ps) ->
    vsep $ flip map ps $ \(r,n,c,vi,vo,act) ->
        planHeader r fa n c (vi,vo) `above` indent 2 (renderDop ddi act)
 where
  planHeader r (fa :: Maybe DFunctAr) n c (vi,vo) =
        text ";;"
    <+> (prettySpanLoc $ r_span r)
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
    <+> text "cost=" <> pretty c
    <+> text "head=" <> pretty (r_head r)
    <+> text "res=" <> pretty (r_result r)

------------------------------------------------------------------------}}}
-- Warnings                                                             {{{

renderSpannedWarn :: BU.ByteString -> [T.Span] -> Doc e
renderSpannedWarn w s = "WARNING:" <+> text (BU.toString w) <+> "AT"
                        `above` indent 2 (vcat (map prettySpanLoc s))

------------------------------------------------------------------------}}}
-- Pipeline!                                                            {{{

processFile :: (?dcfg :: DynacConfig) => String -> IO ()
processFile fileName = bracket openOut hClose go
 where
  openOut = maybe (return stdout) (flip openFile WriteMode)
            $ dcfg_outFile ?dcfg

  maybeWarnANF [] = Nothing
  maybeWarnANF xs = Just $ vcat $ map (uncurry renderSpannedWarn) xs

  go out = do
    rs <- parse

    dump DumpParsed (vcat $ map (text.show) rs)
   
    let urs = map (\(P.LRule x T.:~ _) -> x) rs
        (frs, anfWarns) = unzip $ map normRule urs

    dump DumpANF (vcat $ map printANF frs)

    hPutDoc stderr $ vcat $ MA.mapMaybe maybeWarnANF anfWarns

    aggm <- return $! buildAggMap frs

    dump DumpAgg (M.foldlWithKey (\d f a -> d `above`
                                    (pretty f <+> colon <+> pretty a))
                                 empty aggm)

    case dcfg_backend ?dcfg of
      Backend be_b be_c be_ddi be_d ->
        let initializers = MA.mapMaybe
                             (\(f,mca) -> (\(c,a) -> (f,c,a)) `fmap` mca)
                           $ map (\x -> (x, planInitializer be_b x)) frs
   
  
            uPlans = combineUpdatePlans
                     $ map (\x -> (x, planEachEval be_b
                                                   (flip S.member be_c) x))
                           frs

{-
            qPlans = combineQueryPlans
                     $ map (\x -> (x, planGroundBackchain be_b x))
                           frs
-}

        in do
            -- Force evaluation of a lot of the work of the compiler,
            -- even if the backend and dump flags won't do it for us.
            initializers' <- evaluate $ initializers
            uPlans'       <- evaluate $ uPlans

            dump DumpDopIni (renderDopInis be_ddi initializers')
            dump DumpDopUpd (renderDopUpds be_ddi uPlans')

            -- Invoke the backend code generator
            be_d aggm uPlans' {- qPlans -} initializers' out

  parse = do
    pr <- T.parseFromFileEx (P.rawDLines <* T.eof) fileName
    case pr of
      TR.Failure td -> dynacUserANSIErr $ PPA.align ("Parser error" PPA.<$> td)
      TR.Success rs -> return rs

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
               [Handler printerr, Handler someExnPanic]

 where
  printerr x = pe x >> exitFailure

  pe (UserProgramError d) = do
    PP.hPutDoc stderr (upeMsg <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
  pe (UserProgramANSIError d) = do
    PPA.hPutDoc stderr (upeMsg <> PPA.line <> PPA.indent 1 d)
    hPutStrLn stderr ""
  pe (InvocationError d) = do
    PP.hPutDoc stderr ("Invocation error:" <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
    quickExit QEHelp
  pe (Sorry d) = do
    PP.hPutDoc stderr (sorryMsg <> line <> taMsg <> line <> PP.indent 1 d)
    hPutStrLn stderr ""
  pe (Panic d) = panic d

  someExnPanic (e :: SomeException) = panic $ "Uncaught Haskell exception:"
                                              <+> text (show e)

  panic d = do
    PP.hPutDoc stderr (panicMsg <> line <> taMsg <> line <> PP.indent 1 d)
    hPutStrLn stderr ""

  upeMsg :: (IsString s) => s
  upeMsg = "FATAL: Encountered error in input program:"

  sorryMsg :: (IsString s) => s
  sorryMsg = "Terribly sorry, but you've hit an unsupported feature"

  panicMsg :: (IsString s) => s
  panicMsg = "Compiler panic!"

  taMsg :: (IsString s) => s
  taMsg = "This is almost assuredly not your fault!  Please contact a TA."
------------------------------------------------------------------------}}}
