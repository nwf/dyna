module Dyna.Analysis.Mode.Selftest where

import           Test.Framework
import qualified Dyna.Analysis.Mode.Selftest.NamedInst as NI

selftest = testGroup "Dyna.Analysis.Mode.Selftest" [ NI.selftest ]
