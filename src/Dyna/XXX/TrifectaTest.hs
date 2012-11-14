module Dyna.XXX.TrifectaTest(
	unsafeFS, unsafeFF, unsafeParse, checkParseFail
) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid (mempty)
import           Test.HUnit
import           Text.Trifecta

unsafeFS :: Result t -> t
unsafeFS (Success a) = a
unsafeFS (Failure td) = error $ "Errors: " ++ show td

unsafeFF :: String -> Result t -> Assertion
unsafeFF _ (Success _) = error $ "Unexpected success"
unsafeFF e (Failure td) = e @=? show td

unsafeParse :: (Show a) => (Parser a) -> ByteString -> a
unsafeParse p = unsafeFS . parseByteString (p <* eof) mempty

-- XXX this fails to properly check the last argument of the "Diagnostic"s
-- we get (the [Diagnostic m] argument).  We should fix that eventually.
checkParseFail :: (Show a)
               => Parser a
               -> ByteString
               -> String
               -> Assertion
checkParseFail p i e = unsafeFF e $ parseByteString (p <* eof) mempty i


