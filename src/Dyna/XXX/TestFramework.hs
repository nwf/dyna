module Dyna.XXX.TestFramework where

import           Data.Monoid
import           Test.Framework

moreTests :: Int -> Test -> Test
moreTests n = plusTestOptions 
              $ mempty
                { topt_maximum_generated_tests = Just n
                }


moreTries :: Int -> Test -> Test
moreTries n = plusTestOptions 
              $ mempty
                { topt_maximum_unsuitable_generated_tests = Just n
                }

withTimeout :: Int -> Test -> Test
withTimeout n = plusTestOptions
                $ mempty
                  { topt_timeout = Just $ Just n }
