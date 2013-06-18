---------------------------------------------------------------------------
-- | Tests for context and unification functions.

-- Header material                                                      {{{
{-# LANGUAGE ExplicitForAll #-}
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
import           Dyna.XXX.TestFramework
import qualified Test.Framework                                as TF
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Property                      as QCP
import           Test.Framework.TH

------------------------------------------------------------------------}}}
-- Utility Functions                                                    {{{

unifProp :: forall t a f .
            (Eq a, Ord f)
         => (t -> Bool)
         -> (t -> t -> Either a (NIX f))
         -> (t -> t -> Either a (NIX f))
         -> t
         -> t
         -> Property
unifProp filt test gold i1 i2 =
  filt i1 && filt i2 QCP.==>
  case (test i1 i2, gold i1 i2) of
    (Left t , Left g ) -> t == g
    (Right t, Right g) -> nEq t g
    (_, _)             -> False

unifProp2 :: forall t a f .
            (Eq a, Ord f)
         => (t -> Bool)
         -> (t -> t -> Either a (NIX f))
         -> (t -> t -> Either a (NIX f))
         -> t
         -> t
         -> Property
unifProp2 filt test gold i1 i2 =
  filt i1 && filt i2 QCP.==>
  case (test i1 i2, test i2 i1, gold i1 i2) of
    (Left  t1, Left  t2, Left  g) -> g `elem` [t1,t2]
    (Right t1, Right t2, Right g) -> nEq t1 g || nEq t2 g
    (_       , Left  t , Left  g) -> g == t
    (Left  t , _       , Left  g) -> g == t
    (_       , Right t , Right g) -> nEq t g
    (Right t , _       , Right g) -> nEq t g
    (_, _, _)                     -> False


------------------------------------------------------------------------}}}
-- Tests                                                                {{{

prop_no_unifyUnaliasedNV :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_no_unifyUnaliasedNV = unifProp2 wf no_unifyUnaliasedNV lattice
 where
  wf n = nWFN' n && n `nSub` (nHide $ IUniv UClobbered)

  lattice n1 n2 = fmap (nUpUniq UShared) $ nLeqGLBRL n1 n2

  no_unifyUnaliasedNV n1 n2 =
    fmap fst $ runIdentity
             $ flip CNA.runSIMCT (CNA.ctxFromBindings [(v,n1)])
             $ flip runReaderT (UnifParams True False)
             $ do
                _ <- FNA.unifyUnaliasedNV n2 v
                FNA.expandV v
   where v = "A"

prop_alias_unifyUnaliasedNV :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unifyUnaliasedNV = unifProp nWFN' alias_unifyUnaliasedNV nLeqGLBRL
 where
  alias_unifyUnaliasedNV n1 n2 =
    fmap fst $ runIdentity
             $ flip CA.runSIMCT (CA.ctxFromBindings [(v,n1)])
             $ flip runReaderT (UnifParams True False)
             $ do
                _ <- FA.unifyUnaliasedNV n2 v
                FA.expandV v
   where v = "A"

prop_alias_unify :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unify = unifProp nWFN' alias_unify nLeqGLBRL
 where
  alias_unify n1 n2 =
    fmap fst $ runIdentity
             $ flip CA.runSIMCT (CA.ctxFromBindings [(vA,n1),(vB,n2)])
             $ flip runReaderT (UnifParams True False)
             $ do
                _ <- FA.unifyVV vA vB
                FA.expandV vA
   where vA = "A"
         vB = "B"


-- | Check that a fake unifyVF ascribes to the lattice unification.
-- 
-- We cannot (easily) test real unifications as unifyVF makes a combination
-- of dead and live unifications that do not map onto the lattice functions.
prop_alias_unifyVF :: NIX TestFunct -> NIX TestFunct -> QCP.Property
prop_alias_unifyVF = unifProp nWFN' alias_unifyVF gold
 where
  gold n1 n2 = nLeqGLBRD n1 (nHide $ IBound UUnique
                                             (M.singleton G [n2])
                                             False)

  alias_unifyVF n1 n2 =
    fmap fst $ runIdentity
             $ flip CA.runSIMCT (CA.ctxFromBindings [(vA,n1),(vB,n2)])
             $ do
                _ <- FA.unifyVF True (const $ return True) vA G [vB]
                FA.expandV vA
   where
     vA = "A"
     vB = "B"

------------------------------------------------------------------------}}}
-- Harness Toplevel                                                     {{{

selftest :: TF.Test
selftest = moreTries 1000 $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

------------------------------------------------------------------------}}}
