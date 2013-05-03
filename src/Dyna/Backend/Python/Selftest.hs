---------------------------------------------------------------------------
-- | Self-tests for the Python backend, mostly by running the generated
-- code through the interpreter.

-- Header material                                                      {{{
module Dyna.Backend.Python.Selftest where

import           Control.Exception (throw)
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import qualified Test.Framework                      as TF
import           Test.Golden

------------------------------------------------------------------------}}}
-- Run Backend                                                          {{{

-- XXX There's something wrong here -- if we encounter an ExitFailure and
-- throw an exception, we fail to fail the test or even time out.  This
-- might be my fault, or it might be upstream.
runDynaPy :: String -> String -> IO ()
runDynaPy f out = do
  devnull <- openFile "/dev/null" ReadWriteMode

  (Nothing,Nothing,Nothing,ph) <- createProcess $ CreateProcess
     { cmdspec = RawCommand "/usr/bin/env"
                            ["python", "bin/interpreter.py", "-o", out, f]
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
  let (dy,out,ex) = names in goldenVsFile dy ex out (runDynaPy dy out)
 where
  names = ( "examples/"          ++ name ++ ".dyna"
          , "examples/"          ++ name ++ ".dyna.py.out"
          , "examples/expected/" ++ name ++ ".py.out")

goldens :: TF.Test
goldens = TF.testGroup "Python Backend End-To-End"
          $ map mkExample ["simple", "papa2"]

------------------------------------------------------------------------}}}
-- Harness toplevel                                                     {{{

selftest :: TF.Test
selftest = goldens

main :: IO ()
main = TF.defaultMain [selftest]

-- If you're running from within GHCi and just want to do something quickly,
-- try
--
-- TF.defaultMain [mkExample "simple"]

------------------------------------------------------------------------}}}
