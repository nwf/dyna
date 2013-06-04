module Dyna.XXX.TrifectaTest(
	unsafeFS, unsafeFF, unsafeFF_, unsafeParse, checkParseFail, checkParseFail_
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

unsafeFF :: String -> Result t -> Assertion
unsafeFF _ (Success _) = assertFailure "Unexpected success"
unsafeFF e (Failure td) = e @=? flip PPA.displayS ""
                                     (filterSD $ PPA.renderCompact td)
 where
  -- strip out any ANSI BS
  filterSD PPA.SEmpty = PPA.SEmpty
  filterSD (PPA.SChar c x) = PPA.SChar c (filterSD x)
  filterSD (PPA.SText i s x) = PPA.SText i s (filterSD x)
  filterSD (PPA.SLine i x) = PPA.SLine i (filterSD x)
  filterSD (PPA.SSGR _ x) = filterSD x

unsafeFF_ :: Result t -> Assertion
unsafeFF_ (Success _) = assertFailure "Unexpected success"
unsafeFF_ (Failure _) = return ()

unsafeParse :: (Parser a) -> ByteString -> a
unsafeParse p = unsafeFS . parseByteString (p <* eof) mempty

checkParseFail :: (Show a)
               => Parser a
               -> ByteString
               -> String
               -> Assertion
checkParseFail p i e = unsafeFF e $ parseByteString (p <* eof) mempty i

checkParseFail_ :: Parser a
                -> ByteString
                -> Assertion
checkParseFail_ p i = unsafeFF_ $ parseByteString (p <* eof) mempty i

