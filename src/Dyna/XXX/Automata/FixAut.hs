---------------------------------------------------------------------------
-- | Non-recursive automata from least fixed-points.  The resulting
-- automata may have disjunctions but are not able to recurse to earlier
-- positions (without potentially diverging).

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.XXX.Automata.FixAut where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Trans.Either
import           Data.Functor.Foldable
import qualified Data.Traversable                  as T
import           Dyna.XXX.Automata.ReprClass
import qualified Prelude.Extras                    as PE

------------------------------------------------------------------------}}}
-- Definition                                                           {{{

data FA f = (PE.Ord1 f) => FA { unFA :: Fix f }

------------------------------------------------------------------------}}}
-- AutRepr instance                                                     {{{

instance AutomataRepr FA where
  autHide faf = FA $ Fix $ unFA <$> faf

  autExpose (FA (Fix f)) = FA <$> f

  autMap q (FA f0) = FA (go f0)
   where
    go (Fix f) = Fix (q (go <$> f))

  -- No cycles, so ignore cycle-breaking value
  autReduce _ f = go . unFA
   where
    go (Fix x) = f (fmap go x)

instance AutomataReprBin FA FA where
  autBiReduce _ f (FA l0) (FA r0) = runIdentity (go l0 r0)
   where
    go fl@(Fix l) fr@(Fix r) = f (\l' -> go l' fr) (\r' -> go fl r') go l r

  autGenPMerge cf0 i f (FA l0) (FA r0) =
   FA <$> runIdentity (runEitherT $ go c0 l0 r0)
   where
    c0 = let { (Fix l) = l0; (Fix r) = r0 } in cf0 l r
    go c fl@(Fix l) fr@(Fix r) = do
      er <- f imp imp (\c' l' -> go c' l' fr) (\c' r' -> go c' fl r') go c l r
      Fix <$> hoistEither er
    imp c (Fix x) = Fix <$> (i imp c x >>= hoistEither)

mkAutFromFA :: (AutomataRepr a, PE.Ord1 f, T.Traversable f) => FA f -> a f
mkAutFromFA = autReduce (error "Impossible: mkAutFromFA cycle")
                        autHide

-- | Off-diagonal operations.
--
-- Note that the reverse is not possible because there is no guarantee that
-- the merge of an acyclic automata and a cyclic one is acyclic (whereas in
-- @AutomataReprBin FA a@, 'autGenPMerge' and friends would be obligated to
-- return a @FA@).
instance (AutomataRepr a, AutomataReprBin a a) => AutomataReprBin a FA where
  autBiReduce c f (l :: a f) r = autBiReduce c f l (mkAutFromFA r :: a f)
  autGenPMerge c i m (l :: a f) r = autGenPMerge c i m l (mkAutFromFA r :: a f)

------------------------------------------------------------------------}}}
-- AutomataRender instance                                              {{{

instance AutomataRender FA where
  autRender f a = autReduce (error "Impossible: autRender@FA cycle") f a

------------------------------------------------------------------------}}}
