module Dyna.Analysis.Mode.Selftest where

import           Test.Framework
import qualified Dyna.Analysis.Mode.Selftest.NamedInst as NI
import qualified Dyna.Analysis.Mode.Selftest.Contexts  as C

selftest :: Test
selftest = testGroup "Dyna.Analysis.Mode.Selftest"
                     [ NI.selftest
                     ,  C.selftest
                     ]
