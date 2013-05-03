---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system
--
-- This module implements some basic self-test functionality for the insts
-- predicates in 'Dyna.Analysis.Mode.Insts' using the execution machinery
-- in 'Dyna.Analysis.Mode.Execution.NamedInsts'.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.Analysis.Mode.Selftest.NamedInst where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import qualified Data.Either               as E
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Maybe                as MA
import qualified Data.Set                  as S
import qualified Data.Traversable          as T
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Selftest.Term
import           Dyna.Analysis.Mode.Uniq
import           Dyna.XXX.TestFramework
import qualified Test.Framework            as TF
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck
import           Test.QuickCheck.Property

import qualified Debug.Trace as XT

{-
fromList :: Ord k => [(k,v)] -> M.Map k v
fromList = M.fromList
-}

nWFU = nWellFormedUniq UUnique

-- | Check 'nPrune' using the separate 'arbNIX' generator, rather than
-- the 'Arbitrary' instance, which calls 'nPrune' internally.
prop_nPrune_Eq :: Property
prop_nPrune_Eq = forAll arbNIX (\i -> nWFU i ==> i `nEq` (nPrune i))

-- | This property also checks that we don't generate crazy NIX
-- (such as we used to before the 'resize' call above in 'arbNIXME').
--
-- Notice that this holds regardless of the well-formedness requirements
-- on insts' recursion, since 'nEq' does not actually check them.
prop_nEq_refl :: NIX TestFunct -> Property
prop_nEq_refl i = property $ i `nEq` i

-- XXX These tests are a little more time consuming and possibly not worth
-- the time to run all the time.

prop_nLeq_refl :: NIX TestFunct -> Property
prop_nLeq_refl i = property $ i `nLeq` i

prop_nLeq_asym :: NIX TestFunct -> NIX TestFunct -> Property
prop_nLeq_asym i j = i `nLeq` j && j `nLeq` i ==> i `nEq` j

prop_nSub_refl :: NIX TestFunct -> Property
prop_nSub_refl i = property $ i `nSub` i

prop_nSub_asym :: NIX TestFunct -> NIX TestFunct -> Property
prop_nSub_asym i j = i `nSub` j && j `nSub` i ==> i `nEq` j

-- Check well-formedness of outputs of various binary functions

prop_nLeqGLB_WF :: NIX TestFunct -> NIX TestFunct -> Property
prop_nLeqGLB_WF i1 i2 = nWFU i1 && nWFU i2 ==> nWFU (nLeqGLB i1 i2)

prop_nSubGLB_WF :: NIX TestFunct -> NIX TestFunct -> Property
prop_nSubGLB_WF i1 i2 = nWFU i1 && nWFU i2 ==> nWFU (nSubGLB i1 i2)

-- prop_nSubLUB_WF :: NIX TestFunct -> NIX TestFunct -> Property
-- prop_nSubLUB_WF i1 i2 = nWFU i1 && nWFU i2
--                       ==> maybe True nWFU $ nSubLUB i1 i2

-- Check edge-cases of various binary forms: given inputs ordered by their
-- lattice, these functions return something isomorphic to the appropriate
-- input.

prop_nLeqGLB_Edge :: NIX TestFunct -> NIX TestFunct -> Property
prop_nLeqGLB_Edge i1 i2 =     i1 `nLeq` i2 && nWFU i1 && nWFU i2
                        ==> nLeqGLB i1 i2 `nEq` i1

prop_nSubGLB_Edge :: NIX TestFunct -> NIX TestFunct -> Property
prop_nSubGLB_Edge i1 i2 =     i1 `nSub` i2 && nWFU i1 && nWFU i2
                        ==> nSubGLB i1 i2 `nEq` i1

-- prop_nSubLUB_Edge :: NIX TestFunct -> NIX TestFunct -> Property
-- prop_nSubLUB_Edge i1 i2 =     i1 `nSub` i2 && nWFU i1 && nWFU i2
--                         ==> maybe False (nEq i2) $ nSubLUB i1 i2
    -- Note that we return False above since the `nSub` constraint
    -- should mean that nSubLUB is being called within its domain.

-- Check that the output of a binary function obeys the lattice ordering

prop_nLeqGLB_is_LB :: NIX TestFunct -> NIX TestFunct -> Property
prop_nLeqGLB_is_LB i1 i2 = nWFU i1 && nWFU i2
                       ==> let i = nLeqGLB i1 i2
                           in property $ i `nLeq` i1 && i `nLeq` i2

prop_nLeqGLB_is_G :: NIX TestFunct -> NIX TestFunct -> NIX TestFunct -> Property
prop_nLeqGLB_is_G i1 i2 i3 = nWFU i1 && nWFU i2 && nWFU i3
                       ==> let i = nLeqGLB i1 i2
                           in i3 `nLeq` i1 && i3 `nLeq` i2 ==> property $ i3 `nLeq` i


prop_nSubGLB_is_LB :: NIX TestFunct -> NIX TestFunct -> Property
prop_nSubGLB_is_LB i1 i2 = nWFU i1 && nWFU i2
                       ==> let i = nSubGLB i1 i2
                           in property $ i `nSub` i1 && i `nSub` i2

prop_nSubGLB_is_G :: NIX TestFunct -> NIX TestFunct -> NIX TestFunct -> Property
prop_nSubGLB_is_G i1 i2 i3 = nWFU i1 && nWFU i2 && nWFU i3
                       ==> let i = nSubGLB i1 i2
                           in i3 `nSub` i1 && i3 `nSub` i2 ==> property $ i3 `nSub` i

-- prop_nSubLUB_is_UB :: NIX TestFunct -> NIX TestFunct -> Property
-- prop_nSubLUB_is_UB i1 i2 = nWFU i1 && nWFU i2
--                       ==> maybe (property rejected)
--                                 (\i -> property $ i1 `nSub` i && i2 `nSub` i)
--                           $ nSubLUB i1 i2

-- | When we enact a call, the rules (figure 3.13, p45; see also 'doCall')
-- tell us that we first check that the incoming variables are all ⊑ the
-- input half of the mode, then unify the input variables with the output
-- half of the mode.  We know that the output half is ≼ the input half.
prop_call_test_sufficient :: NIX TestFunct -> NIX TestFunct -> Property
prop_call_test_sufficient i1 i2 =    nWFU i1 && nWFU i2
                                  && nNotEmpty i1 && nNotEmpty i2
                                  && nSub i1 i2
                              ==> nNotEmpty (nLeqGLB i1 i2)

selftest :: TF.Test
selftest = moreTries 10000 $ moreTests 400 $(testGroupGenerator)

main :: IO ()
main = TF.defaultMain [selftest]
