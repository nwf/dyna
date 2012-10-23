---------------------------------------------------------------------------
-- Header material
------------------------------------------------------------------------{{{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Dyna.BackendK3.Selftest where

import           Dyna.BackendK3.AST
import           Dyna.BackendK3.Automation
import           Dyna.BackendK3.Render
import qualified Test.Framework                      as TF
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Match some K3 to strings                                             {{{

  -- | Produce a version of the Doc with minimal formatting
displaySimple :: SimpleDoc e -> ShowS
displaySimple (SEmpty     ) = id
displaySimple (SChar c   d) = showChar c   . displaySimple d
displaySimple (SText _ s d) = showString s . displaySimple d
displaySimple (SLine _   d) = showChar ' ' . displaySimple d
displaySimple (SEffect _ d) =                displaySimple d

render :: AsK3 e a -> String
render = despace . flip displaySimple [] . renderCompact . sh
 where
  despace [] = []
  despace [x] = [x]
  despace (' ':xs@(' ':_)) = despace xs
  despace (' ':xs) = ' ' : despace xs
  despace (x  :xs) = x : despace xs

------------------------------------------------------------------------}}}
-- Basic handling                                                       {{{

case_mfn :: Assertion
case_mfn = e @=? render k3
 where
  e  = "\\x0:int -> -((x0) + (1))"
    -- Note that we cannot automate the tInt here, since K3's math
    -- operators are overloaded, so there's no way to conclude
    -- the type of a from the occurrance of "a + Int".
  k3 = eLam (PVar tInt) (\a -> eNeg $ eAdd a $ cInt 1)

case_pairfn :: Assertion
case_pairfn = e @=? render k3
 where
  e  = "\\(x0:int ,x1:bool) -> x0"
  k3 = eLam (PVar tInt, PVar tBool) (\(a,_) -> a)

------------------------------------------------------------------------}}}
-- Macro expansion test cases                                           {{{

case_mcm :: Assertion
case_mcm = e @=? render k3
 where
  e  =    "if ((test) == (nothing)) then (0) "
       <> "else (((\\just (x0:int) -> x0) (test)))"
  k3 = caseMaybe tInt (unsafeVar (Var "test") autoty) (cInt 0) (id)

------------------------------------------------------------------------}}}
-- Harness toplevel                                                     {{{

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

------------------------------------------------------------------------}}}
