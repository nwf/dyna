{-# LANGUAGE TemplateHaskell #-}

module Dyna.XXX.TrifectaTests (selftest) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString.Char8               as B8
import           Data.Monoid (mempty)
import qualified Test.Framework                      as TF
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit
import qualified Text.PrettyPrint.ANSI.Leijen        as PPA
import           Text.Trifecta

import           Dyna.XXX.Trifecta
import           Dyna.XXX.TrifectaTest

pa :: Parser String
pa = parens (many $ char 'a')

case_incrementality0 :: Assertion
case_incrementality0 =
      unsafeFS (parseByteString pa mempty fullstr)
  @=? unsafeFS (starve (feed fullstr (stepParser (release mempty *> pa) mempty B8.empty)))
 where
   fullstr = B8.pack "(aa)"

case_incrementality1 :: Assertion
case_incrementality1 =
      unsafeFS (parseByteString pa mempty fullstr)
  @=? unsafeFS (starve (feed tstr (feed istr (stepParser (release mempty *> pa) mempty B8.empty))))
 where
   fullstr = B8.concat [istr, tstr]
   istr = B8.pack "(a"
   tstr = B8.pack "a)"

{-
 - XXX no workie
_case_incrementality2 =
      unsafeFS (parseByteString pa mempty fullstr)
  ~=? unsafeFS (starve (feed tstr (stepParser (release mempty *> pa) mempty istr)))
 where
   fullstr = B8.concat [istr, tstr]
   istr = B8.pack "(a"
   tstr = B8.pack "a)"
 -}

interactTest :: (Show a) => Parser a -> [String] -> (Either PPA.Doc a, [String])
interactTest p (i:is) = runState (triInteract p next success failure i) is
 where
  next = do
          l <- get
          case l of
            []   -> return Nothing
            x:xs -> put xs >> return (Just x)

  success = return.Right
  failure = return.Left

successInteract p r i = either (const $ assertFailure "Parser failure")
                               (assertEqual "" r) $
                               fst $ interactTest p i

case_interactOnce = successInteract pa "aa" ["(aa)"]
case_interactMany = successInteract pa "aa" ["", "(a", "", "a)", ""]

selftest :: TF.Test
selftest = $(testGroupGenerator)
