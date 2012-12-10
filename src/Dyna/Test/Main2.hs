-- Bring together all of our test suites

-- XXX temporary, use Dyna.Test.Main as soons as timv has upgrades ghc.

module Dyna.Test.Main2 where

import           Test.Framework
import qualified Dyna.ParserHS.Selftest  as DPHS
import qualified Dyna.XXX.TrifectaTests  as DXT

main :: IO ()
main = defaultMain
           [ DPHS.selftest
           , DXT.selftest
           ]
