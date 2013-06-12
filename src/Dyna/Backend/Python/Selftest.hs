---------------------------------------------------------------------------
-- | Self-tests for the Python backend, mostly by running the generated
-- code through the interpreter.

-- Header material                                                      {{{
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
module Dyna.Backend.Python.Selftest where

import           Control.Exception (throw)
import qualified Dyna.Backend.Python.Backend         as DP
import qualified Dyna.Main.Driver                    as D
import           System.Directory (removeFile)
import           System.Exit (ExitCode(..))
import           System.IO
import           System.IO.Error
import           System.Process
import           Test.Framework                      as TF
import           Test.Framework.Providers.Program
import           Test.Framework.TH
import           Test.Golden

------------------------------------------------------------------------}}}
-- Run Backend                                                          {{{

-- XXX There's something wrong here -- if we encounter an ExitFailure and
-- throw an exception, we fail to fail the test or even time out.  This
-- might be my fault, or it might be upstream.
runDynaPy :: FilePath -> FilePath -> FilePath -> IO ()
runDynaPy f pl out = do
  _ <- tryIOError $ removeFile pl
  _ <- tryIOError $ removeFile out

  let ?dcfg = D.defaultDynacConfig
           { D.dcfg_backend = DP.pythonBackend
           , D.dcfg_outFile = Just pl
           }
   in D.processFile f

  withFile "/dev/null" ReadWriteMode $ \devnull -> do
   (Nothing,Nothing,Nothing,ph) <- createProcess $ CreateProcess
      { cmdspec = RawCommand "/usr/bin/env"
                             [ "python"
                             , "src/Dyna/Backend/Python/interpreter.py"
                             , "--plan"
                             , "-o", out
                             , pl
                             ]
      , cwd = Nothing
      , env = Nothing
      , std_in = UseHandle devnull
      , std_out = UseHandle devnull
      , std_err = UseHandle devnull
      , close_fds = True
      , create_group = False
      }
   ec <- waitForProcess ph
   case ec of
    ExitSuccess -> return ()
    ExitFailure _ -> throw ec

------------------------------------------------------------------------}}}
-- Tests                                                                {{{

mkExample :: String -> TF.Test
mkExample name =
  let (dy,pl,out,ex) = names in goldenVsFile dy ex out (runDynaPy dy pl out)
 where
  names = ( "examples/"          ++ name ++ ".dyna"
          , "examples/"          ++ name ++ ".dyna.py.plan"
          , "examples/"          ++ name ++ ".dyna.py.out"
          , "examples/expected/" ++ name ++ ".py.out")

-- Sorted roughly by likelihood that all subsequent examples
-- will be broken. ;)
test_End_To_End :: [Test]
test_End_To_End = map mkExample
  [ "simple", "fib-limit", "dijkstra", "papa2", "matrixops" ]

test_REPL :: [Test]
test_REPL = map (\n -> testProgramRuns n ("./test/repl/"++n) [])
  [ "aggregator-conflict", "retract-rule" ]

------------------------------------------------------------------------}}}
-- Harness toplevel                                                     {{{

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

-- If you're running from within GHCi and just want to do something quickly,
-- try
--
-- TF.defaultMain [mkExample "simple"]

------------------------------------------------------------------------}}}
