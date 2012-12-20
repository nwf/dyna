---------------------------------------------------------------------------
-- | Self-tests for the Python backend, mostly by running the generated
-- code through the interpreter.

-- Header material                                                      {{{
module Dyna.Backend.Python.Selftest where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy                as BL
import           System.Exit (ExitCode(..))
import           System.IO
import           System.Process
import qualified Test.Framework                      as TF
import           Test.Golden

------------------------------------------------------------------------}}}
-- Run Backend                                                          {{{

runDynaPy :: String -> IO BL.ByteString
runDynaPy f = do
  devnull <- openFile "/dev/null" ReadWriteMode

  (Nothing,Just so,Nothing,ph) <- createProcess $ CreateProcess
     { cmdspec = RawCommand "/usr/bin/env"
                            ["python", "bin/interpreter.py", "-o", "-", f]
     , cwd = Nothing
     , env = Nothing
     , std_in = UseHandle devnull
     , std_out = CreatePipe
     , std_err = UseHandle devnull
     , close_fds = True
     , create_group = False
     }
  bs <- BL.hGetContents so
  ec <- waitForProcess ph
  case ec of
   ExitSuccess -> return bs
   ExitFailure _ -> throw ec

------------------------------------------------------------------------}}}
-- Tests                                                                {{{

mkExample :: String -> TF.Test
mkExample name =
  let (dy,ex) = names in goldenVsString dy ex (runDynaPy dy)
 where
  names = ( "examples/" ++ name ++ ".dyna"
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
