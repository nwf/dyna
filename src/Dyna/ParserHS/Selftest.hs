---------------------------------------------------------------------------
-- | Parser self-test cases
--
-- TODO:
--   Writing these is still too hard, Template Haskell and the REPL
--     notwithstanding.
--
--   Test.Framework.TH appears not to understand block comments at the
--   moment, and parses right through them.

-- Header material                                                      {{{

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Dyna.ParserHS.Selftest where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString                     as B
import qualified Data.ByteString.UTF8                as BU
-- import           Data.Foldable (toList)
-- import           Data.Monoid (mempty)
-- import qualified Data.Sequence                       as S
import           Data.String
import           Dyna.Main.Defns
import           Dyna.ParserHS.Parser
import           Dyna.ParserHS.OneshotDriver
import           Dyna.Term.SurfaceSyntax
import           Dyna.Term.TTerm (Annotation(..), TBase(..))
import           Dyna.XXX.TrifectaTest
import           Test.Framework                      as TF
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit                          as H
import qualified Text.PrettyPrint.Free               as PP
import           Test.QuickCheck                     as Q
import           Text.Trifecta
import           Text.Trifecta.Delta


------------------------------------------------------------------------}}}
-- Terms and basic handling                                             {{{

_tNumeric :: Either Integer Double -> Term
_tNumeric = TBase . TNumeric

defDLC :: DLCfg
defDLC = DLC (mkEOT defOperSpec True) genericAggregators

term :: ByteString -> Spanned Term
term = unsafeParse (testTerm defDLC <* eof)

case_basicAtom :: Assertion
case_basicAtom = e @=? (term "foo")
 where e = TFunctor "foo" [] :~ Span (Columns 0 0) (Columns 3 3) "foo"

case_basicAtomTWS :: Assertion
case_basicAtomTWS = e @=? (term "foo ")
 where e =  TFunctor "foo" [] :~ Span (Columns 0 0) (Columns 4 4) "foo "

case_basicFunctor :: Assertion
case_basicFunctor = e @=? (term sfb)
 where
  e =  TFunctor "foo"
         [TFunctor "bar" [] :~ Span (Columns 4 4) (Columns 7 7) sfb
         ]
        :~ Span (Columns 0 0) (Columns 8 8) sfb

  sfb :: (IsString s) => s
  sfb = "foo(bar)"

case_dollarFunctor :: Assertion
case_dollarFunctor = e @=? (term sfb)
 where
  e =  TFunctor "$foo"
         [TFunctor "bar" [] :~ Span (Columns 5 5) (Columns 8 8) sfb
         ]
        :~ Span (Columns 0 0) (Columns 9 9) sfb

  sfb :: (IsString s) => s
  sfb = "$foo(bar)"

case_nestedFunctorsWithArgs :: Assertion
case_nestedFunctorsWithArgs = e @=? (term st)
 where
  e = TFunctor "foo"
        [TFunctor "bar" [] :~ Span (Columns 4 4) (Columns 7 7) st
        ,TVar "X" :~ Span (Columns 8 8) (Columns 9 9) st
        ,TFunctor "bif" [] :~ Span (Columns 10 10) (Columns 15 15) st
        ,TFunctor "baz"
           [TFunctor "quux" [] :~ Span (Columns 20 20) (Columns 24 24) st
           ,TVar "Y" :~ Span (Columns 25 25) (Columns 26 26) st
           ]
          :~ Span (Columns 16 16) (Columns 27 27) st
        ]
       :~ Span (Columns 0 0) (Columns 28 28) st

  st :: (IsString s) => s
  st = "foo(bar,X,bif(),baz(quux,Y))"

case_basicFunctorComment :: Assertion
case_basicFunctorComment = e @=? (term sfb)
 where
  e =  TFunctor "foo" [] :~ Span (Columns 0 0) (Columns 8 8) sfb

  sfb :: (IsString s) => s
  sfb = "foo %xxx"

case_basicFunctorNLComment :: Assertion
case_basicFunctorNLComment = e @=? (term sfb)
 where
  e =  TFunctor "foo"
         [_tNumeric (Left 1) :~ Span (Lines 1 0 9 0) (Lines 1 1 10 1) "1,2\n"
         ,_tNumeric (Left 2) :~ Span (Lines 1 2 11 2) (Lines 2 0 13 0) "1,2\n"
         ]
        :~ Span (Columns 0 0) (Lines 2 1 14 1) "foo(%xxx\n"

  sfb :: (IsString s) => s
  sfb = "foo(%xxx\n1,2\n)"

case_basicFunctorTWS :: Assertion
case_basicFunctorTWS = e @=? (term sfb)
 where
  e = TFunctor "foo"
       [TFunctor "bar" [] :~ Span (Columns 5 5) (Columns 9 9) sfb
       ] :~ Span (Columns 0 0) (Columns 10 10) sfb

  sfb :: (IsString s) => s
  sfb = "foo (bar )"

case_basicFunctorNL :: Assertion
case_basicFunctorNL = e @=? (term sfb)
 where
  e = TFunctor "foo"
       [TFunctor "bar" [] :~ Span (Lines 1 1 5 1) (Lines 1 5 9 5) "(bar )"
       ] :~ Span (Columns 0 0) (Lines 1 6 10 6) "foo\n"

  sfb :: (IsString s) => s
  sfb = "foo\n(bar )"

case_colonFunctor :: Assertion
case_colonFunctor = e @=? (term pvv)
 where
  e = TFunctor "possible"
        [TFunctor ":"
           [TVar "Var" :~ Span (Columns 9 9) (Columns 12 12) pvv
           ,TVar "Val" :~ Span (Columns 13 13) (Columns 16 16) pvv
           ]
          :~ Span (Columns 9 9) (Columns 16 16) pvv
        ]
       :~ Span (Columns 0 0) (Columns 17 17) pvv
  pvv = "possible(Var:Val)"

-- case_nullaryStar :: Assertion
-- case_nullaryStar = e @=? (term gs)
--  where
--   e  = TFunctor "gensym"
--          [TFunctor "*" [] :~ Span (Columns 7 7) (Columns 8 8) gs
--          ] :~ Span (Columns 0 0) (Columns 9 9) gs
--   gs = "gensym(*)"

case_failIncompleteExpr :: Assertion
case_failIncompleteExpr = checkParseFail (testTerm defDLC) "foo +"
  "(interactive):1:5: error: expected: \"(\",\nend of input\nfoo +<EOF> \n    ^      "

------------------------------------------------------------------------}}}
-- Annotations                                                          {{{

case_tyAnnot :: Assertion
case_tyAnnot = e @=? (term fintx)
 where
  e = TFunctor "f" [TAnnot (AnnType $ TFunctor "int" []
                                     :~ Span (Columns 3 3) (Columns 7 7) fintx)
                           (TVar "X" :~ Span (Columns 7 7) (Columns 8 8) fintx)
                     :~ Span (Columns 2 2) (Columns 8 8) fintx
                   ]
                  :~ Span (Columns 0 0) (Columns 9 9) fintx
  fintx = "f(:int X)"

------------------------------------------------------------------------}}}
-- Aggregators                                                          {{{

test_aggregators :: [TF.Test]
test_aggregators = hUnitTestToTests $ TestList
  [ TestLabel "generic valid" $ TestList $
      map (\x -> (BU.toString x) ~: x ~=? unsafeParse testGenericAggr x)
        ["+=", "*=", ".=", "min=", "max=", "?=", ":-", "max+=" ]
  , TestLabel "generic invalid" $ TestList $
      map (\x -> TestLabel (BU.toString x) $ TestCase
                                           $ checkParseFail_ testGenericAggr x)
        [".", ". ", "+=3", "+3=", "+=a", "+a=" ]
  , TestLabel "custom accept" $
      let r = unsafeParse (testRule cdlc) r1
      in r ~=? Rule (TFunctor "a" [] :~ Span (Columns 0 0) (Columns 2 2) r1)
                    "+="
                    (TFunctor "b" [] :~ Span (Columns 5 5) (Columns 6 6) r1)
  , TestLabel "custom reject" $ TestCase
                              $ checkParseFail_ (testRule cdlc) "a *= b."
  ]
 where
  r1 = "a += b."

  cdlc = DLC { dlc_opertab = dlc_opertab defDLC
             , dlc_aggrs   = fmap BU.fromString (string "+=")
             }

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

progrule :: ByteString -> Spanned Rule
progrule = unsafeParse (spanned (testRule defDLC <* eof))

progrules :: ByteString -> [Spanned Rule]
progrules = unsafeParse (many (spanned (testRule defDLC)) <* eof)

oneshotRules :: ByteString -> [(RuleIx, Spanned Rule)]
oneshotRules = xlate . unsafeParse (oneshotDynaParser Nothing)
 where
  xlate (PDP rs _) = map (\(i,_,sr) -> (i,sr)) rs

case_ruleFact :: Assertion
case_ruleFact = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 4 4) sr)
       "|="
       (TFunctor "true" [] :~ Span (Columns 0 0) (Columns 4 4) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 5 5) sr
  sr = "goal."

case_ruleSimple :: Assertion
case_ruleSimple = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
       "+="
       (_tNumeric (Left 1) :~ Span (Columns 8 8) (Columns 9 9) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 10 10) sr
  sr = "goal += 1."

case_ruleSimple_funny_spacing :: Assertion
case_ruleSimple_funny_spacing = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
       "+="
       (TFunctor "a" [] :~ Span (Columns 7 7) (Columns 8 8) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 9 9) sr
  sr = "goal +=a."

case_ruleSimple_no_spaces :: Assertion
case_ruleSimple_no_spaces = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 4 4) sr)
       "+="
       (TFunctor "a" [] :~ Span (Columns 6 6) (Columns 7 7) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 8 8) sr
  sr = "goal+=a."


case_ruleSimple0 :: Assertion
case_ruleSimple0 = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
       "+="
       (_tNumeric (Left 0) :~ Span (Columns 8 8) (Columns 9 9) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 10 10) sr
  sr = "goal += 0."

case_ruleExpr :: Assertion
case_ruleExpr = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
       "+="
       (TFunctor "+"
            [TFunctor "foo" [] :~ Span (Columns 8 8) (Columns 12 12) sr
            ,TFunctor "bar" [] :~ Span (Columns 14 14) (Columns 18 18) sr
            ]
          :~ Span (Columns 8 8) (Columns 18 18) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 19 19) sr
  sr = "goal += foo + bar ."

case_ruleDotExpr :: Assertion
case_ruleDotExpr = e @=? (progrule sr)
 where
  e  = Rule
       (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
       "+="
       (TFunctor "."
            [TFunctor "foo" [] :~ Span (Columns 8 8) (Columns 11 11) sr
            ,TFunctor "bar" [] :~ Span (Columns 12 12) (Columns 15 15) sr
            ]
           :~ Span (Columns 8 8) (Columns 15 15) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 16 16) sr
  sr = "goal += foo.bar."

case_ruleComma :: Assertion
case_ruleComma = e @=? (progrule sr)
 where
  e =  Rule
       (TFunctor "foo" [] :~ Span (Columns 0 0) (Columns 4 4) sr)
       "+="
       (TFunctor "," [TFunctor "bar" [TVar "X" :~ Span (Columns 11 11) (Columns 12 12) sr]
                                     :~ Span (Columns 7 7) (Columns 13 13) sr
         ,TFunctor "," [TFunctor "baz" [TVar "X" :~ Span (Columns 19 19) (Columns 20 20) sr]
                                      :~ Span (Columns 15 15) (Columns 21 21) sr
          ,TVar "X" :~ Span (Columns 23 23) (Columns 24 24) sr]
         :~ Span (Columns 15 15) (Columns 24 24) sr]
        :~ Span (Columns 7 7) (Columns 24 24) sr)
      :~ ts
  ts = Span (Columns 0 0) (Columns 25 25) sr
  sr = "foo += bar(X), baz(X), X."

case_ruleKeywordsComma :: Assertion
case_ruleKeywordsComma = e @=? (progrule sr)
 where
  e = Rule
      (TFunctor "foo" [] :~ Span (Columns 0 0) (Columns 4 4) sr)
      "="
      (TFunctor "whenever" [TFunctor "new" [TVar "X" :~ Span (Columns 10 10) (Columns 12 12) sr]
                             :~ Span (Columns 6 6) (Columns 12 12) sr
          ,TFunctor "," [TFunctor "is" [TVar "X" :~ Span (Columns 21 21) (Columns 23 23) sr
                                       ,TFunctor "baz" [TVar "Y" :~ Span (Columns 30 30) (Columns 31 31) sr]
                                         :~ Span (Columns 26 26) (Columns 32 32) sr]
                           :~ Span (Columns 21 21) (Columns 32 32) sr
                                       ,TFunctor "is" [TVar "Y" :~ Span (Columns 34 34) (Columns 36 36) sr
                                                      ,_tNumeric (Left 3) :~ Span (Columns 39 39) (Columns 41 41) sr]
                                         :~ Span (Columns 34 34) (Columns 41 41) sr]
             :~ Span (Columns 21 21) (Columns 41 41) sr] -- End "whenever"
            :~ Span (Columns 6 6) (Columns 41 41) sr) -- End expression
      :~ ts
  ts = Span (Columns 0 0) (Columns 42 42) sr
  sr = "foo = new X whenever X is baz(Y), Y is 3 ."

case_rules :: Assertion
case_rules = e @=? (progrules sr)
 where
  e = [ Rule
        (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
        "+="
        (_tNumeric (Left 1) :~ Span (Columns 8 8) (Columns 10 10) sr)
       :~ s1
      , Rule
        (TFunctor "laog" [] :~ Span (Columns 12 12) (Columns 17 17) sr)
        "min="
        (_tNumeric (Left 2) :~ Span (Columns 22 22) (Columns 24 24) sr)
       :~ s2
      ]
  s1 = Span (Columns 0 0) (Columns 11 11) sr
  s2 = Span (Columns 11 11) (Columns 25 25) sr
  sr = "goal += 1 . laog min= 2 ."

case_rules_ruleix_pragmas :: Assertion
case_rules_ruleix_pragmas = e @=? (oneshotRules sr)
 where
  e = [ ( 5
        , Rule
          (TFunctor "goal" [] :~ Span (Columns 13 13) (Columns 18 18) sr)
          "+="
          (_tNumeric (Left 1) :~ Span (Columns 21 21) (Columns 22 22) sr)
         :~ s1
        )
      , ( 6
        , Rule
          (TFunctor "laog" [] :~ Span (Columns 24 24) (Columns 29 29) sr)
          "min="
          (_tNumeric (Left 2) :~ Span (Columns 34 34) (Columns 35 35) sr)
        :~ s2
        )
      ]

  s1 = Span (Columns 13 13) (Columns 23 23) sr
  s2 = Span (Columns 24 24) (Columns 36 36) sr
  sr = ":- ruleix 5. goal += 1. laog min= 2."
 
 
case_rulesWhitespace :: Assertion
case_rulesWhitespace = e @=? (progrules sr)
 where
  e  = [ Rule
         (TFunctor "goal" [] :~ Span (Columns 2 2) (Lines 1 1 16 1) l0)
         "+="
         (_tNumeric (Left 1) :~ Span (Lines 1 4 19 4) (Lines 1 6 21 6) l1)
        :~ s1
       , Rule
         (TFunctor "goal" [] :~ Span (Lines 3 1 31 1) (Lines 3 6 36 6) l3)
         "+="
         (_tNumeric (Left 2) :~ Span (Lines 3 9 39 9) (Lines 3 11 41 11) l3)
        :~ s2
       ]
  l0 = "  goal%comment\n"
  l1 = " += 1 .\n"
  l2 = "%test \n"
  l3 = " goal += 2 ."
  s1 = Span (Columns 0 0) (Lines 1 7 22 7) l0
  s2 = Span (Lines 1 7 22 7) (Lines 3 12 42 12) l1
  sr = B.concat [l0,l1,l2,l3]

case_rulesDotExpr :: Assertion
case_rulesDotExpr = e @=? (progrules sr)
 where
  e  = [ Rule
         (TFunctor "goal" [] :~ Span (Columns 0 0) (Columns 5 5) sr)
         "+="
         (TFunctor "."
              [TFunctor "foo" [] :~ Span (Columns 8 8) (Columns 11 11) sr
              ,TFunctor "bar" [] :~ Span (Columns 12 12) (Columns 15 15) sr
              ]
             :~ Span (Columns 8 8) (Columns 15 15) sr)
        :~ s1
       , Rule
         (TFunctor "goal" [] :~ Span (Columns 17 17) (Columns 22 22) sr)
         "+="
         (_tNumeric (Left 1) :~ Span (Columns 25 25) (Columns 27 27) sr)
        :~ s2
       ]
  s1 = Span (Columns 0 0) (Columns 16 16) sr
  s2 = Span (Columns 16 16) (Columns 28 28) sr
  sr = "goal += foo.bar. goal += 1 ."

------------------------------------------------------------------------}}}
-- Pragmas                                                              {{{

arbPragma :: Gen Pragma
arbPragma = oneof
  [ PDispos <$> arbSD <*> arbAtom <*> listOf arbAD
  , PDisposDefl <$> elements ["dyna", "prologish"]
  ]
 where
  arbSD = elements [SDInherit, SDEval, SDQuote]
  arbAD = elements [ADEval, ADQuote]

  arbAtom = elements [ "f", "+" ]

prop_pragma_roundtrip :: Property
prop_pragma_roundtrip = 
  forAll arbPragma (\p -> p == unsafeParse (testPragma defDLC)
                      (BU.fromString
                      (flip PP.displayS "" $ PP.renderCompact
                                           $ renderPragma p)))

------------------------------------------------------------------------}}}
-- Harness toplevel                                                     {{{

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

------------------------------------------------------------------------}}}
