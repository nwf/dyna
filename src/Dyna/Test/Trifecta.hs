{-# LANGUAGE Rank2Types #-}

module Dyna.Test.Trifecta where

import           Control.Applicative ((<*),(*>))
import           Data.ByteString (ByteString)
import           Data.Foldable (toList)
import           Data.Monoid (mempty)
import qualified Data.Sequence                       as S
import           Data.String
import           Test.HUnit
import           Text.Trifecta
import           Text.Trifecta.Diagnostic.Rendering.Prim (Rendering(..))

unsafeParse :: (Show a) => (forall r . (Parser r String a)) -> ByteString -> a
unsafeParse p = unsafeFS . parseByteString (p <* eof) mempty
 where unsafeFS (Success xs s) | S.null xs = s
       unsafeFS (Success xs _) = error $ "Warnings: " ++ show (toList xs)
       unsafeFS (Failure xs) = error $ "Errors: " ++ show (toList xs)

-- XXX this fails to properly check the last argument of the "Diagnostic"s
-- we get (the [Diagnostic m] argument).  We should fix that eventually.
checkParseFail :: (Show a)
               => (forall r . (Parser r String a))
               -> ByteString
               -> [(Either String Delta, String)]
               -> Assertion
checkParseFail p i e = unsafeFF e $ parseByteString (p <* eof) mempty i
 where
  unsafeFF e (Success _ _) = error $ "Unexpected success"
  unsafeFF e (Failure xs) = e @=? map extractDiag (toList xs)
  extractDiag (Diagnostic (Left s) _ m _) = (Left s, show m)
  extractDiag (Diagnostic (Right (Rendering d _ _ _ _)) _ m _) = (Right d, show m)



