-- Bring together all of our test suites

module Dyna.Test.Main where

import           Test.Framework
import qualified Dyna.ParserHS.ParserSelftest as DPHS
import qualified Dyna.XXX.TrifectaTests       as DXT

main :: IO ()
main = defaultMain
           [DPHS.selftest
           , DXT.selftest
           ]
