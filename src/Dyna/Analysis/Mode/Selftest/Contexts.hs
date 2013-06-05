---------------------------------------------------------------------------
-- | Tests for context and unification functions.

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Dyna.Analysis.Mode.Selftest.Contexts where

import           Control.Monad.Reader
import           Data.Functor.Identity
import qualified Data.Map                                      as M
import qualified Dyna.Analysis.Mode.Execution.Context          as CA
import qualified Dyna.Analysis.Mode.Execution.ContextNoAlias   as CNA
import qualified Dyna.Analysis.Mode.Execution.Functions        as FA
import qualified Dyna.Analysis.Mode.Execution.FunctionsNoAlias as FNA
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Selftest.NamedInst
import           Dyna.Analysis.Mode.Selftest.Term
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import qualified Test.Framework                                as TF
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Property                      as QCP
import           Test.Framework.TH

------------------------------------------------------------------------}}}
-- Utility Functions                                                    {{{


unifProp filter test gold i1 i2 =
  filter i1 && filter i2 QCP.==>
  case (test i1 i2, gold i1 i2) of
    (Left t , Left g ) -> t == g
    (Right t, Right g) -> nEq t g
    (_, _)             -> False

------------------------------------------------------------------------}}}
-- Tests                                                                {{{

prop_alias_unifyUnaliasedNV :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unifyUnaliasedNV = unifProp nWFN' alias_unifyUnaliasedNV nLeqGLBRL
 where
  alias_unifyUnaliasedNV n1 n2 =
    fmap fst $ runIdentity
             $ flip CA.runSIMCT (CA.allFreeSIMCtx [v])
             $ flip runReaderT (UnifParams True False)
             $ do
                FA.unifyUnaliasedNV n1 v
                FA.unifyUnaliasedNV n2 v
                FA.expandV v
   where v = "A"

prop_alias_unify :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unify = unifProp nWFN' alias_unify nLeqGLBRL
 where
  alias_unify n1 n2 =
    fmap fst $ runIdentity
             $ flip CA.runSIMCT (CA.allFreeSIMCtx [vA,vB])
             $ flip runReaderT (UnifParams True False)
             $ do
                FA.unifyUnaliasedNV n1 vA
                FA.unifyUnaliasedNV n2 vB
                FA.unifyVV vA vB
                FA.expandV vA
   where vA = "A"
         vB = "B"


-- | Check that a fake unifyVF ascribes to the lattice unification.
-- 
-- We cannot (easily) test real unifications as unifyVF makes a combination
-- of dead and live unifications that do not map onto the lattice functions.
prop_alias_unifyVF :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unifyVF = unifProp nWFN' alias_unifyVF gold

gold n1 n2 = nLeqGLBRD n1 (nHide $ IBound UUnique
                                             (M.singleton G [n2])
                                             False)

alias_unifyVF n1 n2 =
  fmap fst $ runIdentity
           $ flip CA.runSIMCT (CA.allFreeSIMCtx [vA,vB])
           $ do
              _ <- flip runReaderT (UnifParams True False) $ do
                _ <- FA.unifyUnaliasedNV n1 vA
                FA.unifyUnaliasedNV n2 vB
              _ <- FA.unifyVF True (const $ return True) vA G [vB]
              FA.expandV vA
 where
   vA = "A"
   vB = "B"

------------------------------------------------------------------------}}}
-- Harness Toplevel                                                     {{{

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

------------------------------------------------------------------------}}}
