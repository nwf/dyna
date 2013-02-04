---------------------------------------------------------------------------
-- | Main driver of the pipeline

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams #-} -- This is probably a terrible idea, but
                                -- I'd never done it before and wanted to
                                -- see what it's like.  It won't be hard to
                                -- rip out.
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Main.Driver where

import           Control.Applicative ((<*))
import           Control.Exception
-- import           Control.Monad
import           Data.Char
import qualified Data.Map                     as M
import qualified Data.Maybe                   as MA
import qualified Data.Set                     as S
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.ANF
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.Python
import           Dyna.Main.BackendDefn
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Parser         as P
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.PrettyPrint.Free
import qualified Text.PrettyPrint.ANSI.Leijen as PPA
import qualified Text.Trifecta                as T
import qualified Text.Trifecta.Result         as TR

------------------------------------------------------------------------}}}
-- Dumping                                                              {{{

data DumpType = DumpAgg
              | DumpANF
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
     then    header `above` doc <> line <> line
          <> hcat (replicate 4 bar) <> line
     else doc

  header = bar <+> fill 18 (text $ show dt) <+> bar
  bar    = "=========="

anyDumpStderr :: (?dcfg :: DynacConfig) => Bool
anyDumpStderr = M.foldr (\v r -> r || MA.isNothing v)
                        False (dcfg_dumps ?dcfg)

------------------------------------------------------------------------}}}
-- Backend                                                              {{{

noBackend :: Backend
noBackend = Backend
          { be_builtin   = \_ -> Left False
          , be_constants = S.empty
          , be_driver    = \_ _ _ _ _ -> hPutStrLn stderr
                                          "No backend specified; stopping"
          }

backendmap :: M.Map String (Doc e,Backend)
backendmap = M.fromList
             [ ("none", ("null backend for early stages only", noBackend))
             , ("python", ("generate python code",pythonBackend))
             ]

parseBackend :: String -> Backend
parseBackend s = maybe (dynacThrow $ InvocationError
                                   $ "Unknown backend:" <+> pretty s)
                       snd
                       $ M.lookup (map toLower s) backendmap

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
  putDoc $ above "\nBackends available: "
           $ indent 4 $ vcat
                      $ map (\(k,v) -> pretty k <+> colon <+> fst v)
                      $ M.assocs backendmap
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

dumpOpts :: Bool -> [OptDescr Opt]
dumpOpts nos =
     mkDumpOpt "agg"   DumpAgg     "Aggregator summary"
  ++ mkDumpOpt "anf"   DumpANF     "Administrative Normal Form"
  ++ mkDumpOpt "parse" DumpParsed  "Parser output"
 where
  mkDumpOpt arg fl hm =
    Option [] ["dump-"    ++ arg] (OptArg (OptDump . sfl) "FILE") hm
    : if nos
       then [ Option [] ["no-dump-" ++ arg] (NoArg (OptDump cfl)) "" ]
       else []
   where
    sfl x fs = M.insert fl x fs
    cfl   fs = M.delete fl   fs

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
-- Pipeline!                                                            {{{

processFile :: (?dcfg :: DynacConfig) => String -> IO ()
processFile fileName = bracket openOut hClose go
 where
  openOut = maybe (return stdout) (flip openFile WriteMode)
            $ dcfg_outFile ?dcfg

  go out = do
    rs <- parse

    dump DumpParsed (vcat $ map (text.show) rs)
   
    let urs = map (\(P.LRule x T.:~ _) -> x) rs
        frs = map normRule urs

    dump DumpANF (vcat $ map printANF frs)

    aggm <- return $! buildAggMap frs

    dump DumpAgg (M.foldlWithKey (\d f a -> d `above`
                                    (pretty f <+> colon <+> pretty a))
                                 empty aggm)

    case dcfg_backend ?dcfg of
      Backend be_b be_c be_d ->
        let initializers = MA.mapMaybe
                             (\(f,mca) -> (\(c,a) -> (f,c,a)) `fmap` mca)
                           $ map (\x -> (x, planInitializer be_b x)) frs
   
  
            cPlans = combineUpdatePlans
                     $ map (\x -> (x, planEachEval be_b
                                                   (flip S.member be_c) x))
                           frs

            qPlans = combineQueryPlans
                     $ map (\x -> (x, planGroundBackchain be_b x))
                           frs

        in be_d aggm cPlans qPlans initializers out

  parse = do
    pr <- T.parseFromFileEx (P.dlines <* T.eof) fileName
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
main = getArgs >>= main_

------------------------------------------------------------------------}}}
