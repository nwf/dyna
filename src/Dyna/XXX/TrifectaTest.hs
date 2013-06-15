module Dyna.XXX.TrifectaTest(
	unsafeFS, unsafeFF, unsafeParse, checkParseFail,
) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Monoid (mempty)
import           Test.HUnit
import           Text.Trifecta
import qualified Text.PrettyPrint.ANSI.Leijen as PPA

unsafeFS :: Result t -> t
unsafeFS (Success a) = a
unsafeFS (Failure td) = error $ "Errors: " ++ show td

unsafeFF :: (String -> Assertion) -> Result t -> Assertion
unsafeFF _ (Success _) = assertFailure "Unexpected success"
unsafeFF a (Failure td) = a $ flip PPA.displayS ""
                               (filterSD $ PPA.renderCompact td)
 where
  -- strip out any ANSI BS
  filterSD PPA.SEmpty = PPA.SEmpty
  filterSD (PPA.SChar c x) = PPA.SChar c (filterSD x)
  filterSD (PPA.SText i s x) = PPA.SText i s (filterSD x)
  filterSD (PPA.SLine i x) = PPA.SLine i (filterSD x)
  filterSD (PPA.SSGR _ x) = filterSD x

unsafeParse :: (Parser a) -> ByteString -> a
unsafeParse p = unsafeFS . parseByteString (p <* eof) mempty

checkParseFail :: (Show a)
               => Parser a
               -> ByteString
               -> (String -> Assertion)
               -> Assertion
checkParseFail p i e = unsafeFF e $ parseByteString (p <* eof) mempty i

