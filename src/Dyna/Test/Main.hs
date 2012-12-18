-- Bring together all of our test suites

module Dyna.Test.Main where

import           Test.Framework
import qualified Dyna.Backend.K3.Selftest as DK3S
import qualified Dyna.ParserHS.Selftest   as DPHS
import qualified Dyna.XXX.TrifectaTests   as DXT

main :: IO ()
main = defaultMain
           [DPHS.selftest
           -- XXX Until this is meaningful...
           -- ,DK3S.selftest
           , DXT.selftest
           ]
