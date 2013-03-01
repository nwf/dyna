---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system
--
-- This module implements some basic self-test functionality for the insts
-- predicates in 'Dyna.Analysis.Mode.Insts'.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.Analysis.Mode.InstSelftest where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import qualified Data.Map                  as M
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import qualified Test.Framework            as TF
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.QuickCheck

newtype Fix f = In { out :: f (Fix f) }
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Ord f, Arbitrary f) => Arbitrary (Fix (InstF f))

instance Arbitrary Uniq where arbitrary = arbitraryBoundedEnum
instance (Ord f, Arbitrary f, Arbitrary i) => Arbitrary (InstF f i) where
  arbitrary = frequency [ (1,pure IFree)
                        , (1,IAny   <$> arbitrary)
                        , (1,IUniv  <$> arbitrary)
                        , (3,IBound <$> arbitrary
                                    <*> sized (\s -> resize (s`div`2)
                                              (M.fromList <$> arbitrary))
                                    <*> arbitrary)
                    ]


q1 f = \x y -> f (\a ob -> q1 f a (In ob))
                 (\a b  -> q1 f a b)
                 (out x) (out y)

qtb f = \x y -> In <$> f (\a b -> qtb f a b) (out x) (out y)

qpb :: (Monad m, ff ~ Fix f, fff ~ f ff)
    => ((ff -> ff -> MaybeT m ff)
        -> fff -> fff -> MaybeT m (Maybe fff))
    -> ff -> ff -> m (Maybe ff)
qpb f x y = runMaybeT $ q x y
 where q a b = maybe mzero (return . In) =<< f q (out a) (out b)

qdLeq    a b = runIdentity $ q1  iLeq_    a b 
qdLeqGLB a b = runIdentity $ qtb (iLeqGLB_ return return) a b
qdSub    a b = runIdentity $ q1  iSub_    a b
qdSubGLB a b = runIdentity $ qtb (iSubGLB_ return return) a b
qdSubLUB a b = runIdentity $ qpb (iSubLUB_ return return) a b

prop_iLeq_GLB :: Fix (InstF Bool) -> Fix (InstF Bool) -> Property
prop_iLeq_GLB i1 i2 =     i1 `qdLeq` i2
                      ==> qdLeqGLB i1 i2 == i1

prop_iSub_GLB :: Fix (InstF Bool) -> Fix (InstF Bool) -> Property
prop_iSub_GLB i1 i2 =     i1 `qdSub` i2
                      ==> qdSubGLB i1 i2 == i1

prop_iSub_LUB :: Fix (InstF Bool) -> Fix (InstF Bool) -> Property
prop_iSub_LUB i1 i2 =     i1 `qdSub` i2
                      ==> qdSubLUB i1 i2 == (Just i2)

selftest :: TF.Test
selftest = $(testGroupGenerator)

main :: IO ()
main = $(defaultMainGenerator)

