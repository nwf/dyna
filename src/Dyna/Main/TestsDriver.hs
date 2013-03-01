-- | Bring together all of our test suites
module Dyna.Main.TestsDriver where

import           Data.Monoid
import           Test.Framework
import qualified Dyna.Analysis.Mode.InstSelftest  as DAMI
-- import qualified Dyna.Backend.K3.Selftest     as DBK3S
import qualified Dyna.Backend.Python.Selftest     as DBPS
import qualified Dyna.ParserHS.Selftest           as DPHS
import qualified Dyna.XXX.TrifectaTests           as DXT

main :: IO ()
main = defaultMain
           [DPHS.selftest
           , DXT.selftest
           , moreTries 2000 $ DAMI.selftest

           -- XXX Until this is meaningful...
           -- ,DBK3S.selftest

           ,DBPS.selftest
           ]

moreTries :: Int -> Test -> Test
moreTries n = plusTestOptions 
              $ mempty
                { topt_maximum_unsuitable_generated_tests = Just n
                }
