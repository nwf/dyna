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
import           Control.Monad
import           Data.Char
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Set                   as S
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.ANF
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.Python
import           Dyna.Main.BackendDefn
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Parser       as P
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T

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
          , be_driver    = \_ _ _ _ -> hPutStrLn stderr
                                        "No backend specified; stopping"
          }

parseBackend :: String -> Backend
parseBackend s = case map toLower s of
                   "none" -> noBackend
                   "python" -> pythonBackend
                   _ -> dynacThrow $ InvocationError
                                   $ "Unknown backend:" <+> pretty s

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

data Opt = OptVersion
         | OptHelp
         | OptBackend Backend
         | OptDump (DumpMap -> DumpMap)
         | OptOutput  FilePath


options :: [OptDescr Opt]
options =
  [ Option ['h'] ["help"]    (NoArg  OptHelp)    "display this help message"
  , Option ['V'] ["version"] (NoArg  OptVersion) "display version and exit"
  ]
  ++
  [ Option ['B'] ["backend"]      (ReqArg obe "BE")
    "use backend BE"
  , Option ['o'] ["out","output"] (ReqArg OptOutput "FILE")
    "write generated output to FILE"
  ]
  -- XXX we'd like these to not be documented, at least not by default, but
  -- that would require patching the getopt library
  ++ mkDumpOpt "agg"   DumpAgg
  ++ mkDumpOpt "anf"   DumpANF
  ++ mkDumpOpt "parse" DumpParsed
 where
  obe = OptBackend . parseBackend

  mkDumpOpt arg fl =
    [ Option [] ["dump-"    ++ arg] (OptArg (OptDump . sfl) "FILE") ""
    , Option [] ["no-dump-" ++ arg] (NoArg  (OptDump   cfl)       ) ""
    ]
   where
    sfl x fs = M.insert fl x fs
    cfl   fs = M.delete fl   fs


procArgs :: [String] -> IO (DynacConfig, [String])
procArgs argv = do
  case getOpt Permute options argv of
    (os,as,[]) -> case foldOpts os of
                    Left x -> do
                               putStrLn "Dyna 0.4"
                               when x $ putStrLn (usageInfo "" options)
                               exitSuccess
                    Right f -> return $ (f defaultDynacConfig, as)
    (_,_,es)   -> dynacThrow $ InvocationError
                             $ vcat (map pretty es)
                                `above`
                                pretty (usageInfo "" options)
 where
  foldOpts :: [Opt] -> Either Bool (DynacConfig -> DynacConfig)
  foldOpts = go id
   where
    go f []                = Right f
    go _ (OptHelp:_)       = Left True
    go _ (OptVersion:_)    = Left False
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

    aggm <- case buildAggMap frs of
              Left e -> dynacThrow $ UserProgramError (text e)
              Right x -> return x

    dump DumpAgg (M.foldlWithKey (\d f a -> d `above`
                                    (pretty f <+> colon <+> pretty a))
                                 empty aggm)

    case dcfg_backend ?dcfg of
      Backend be_b be_c be_d ->
        let initializers = MA.mapMaybe
                             (\(f,mca) -> (\(c,a) -> (f,c,a)) `fmap` mca)
                           $ map (\x -> (x, planInitializer be_b x)) frs
   
  
            cPlans = combinePlans
                     $ map (\x -> (x, planEachEval be_b
                                                   (not . flip S.member be_c) x))
                           frs
        in be_d aggm cPlans initializers out

  parse = do
    pr <- T.parseFromFileEx (P.dlines <* T.eof) fileName
    case pr of
      T.Failure td -> dynacUserErr $ align ("Parser error" `above` td)
      T.Success rs -> return rs

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
