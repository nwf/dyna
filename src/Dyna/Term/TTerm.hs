---------------------------------------------------------------------------
-- | Very, very basic (Trivial, even) representation of terms.
--
-- XXX This isn't going to be sufficient when we start doing more
-- complicated things, but it suffices for now?

-- Header material                                                      {{{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Dyna.Term.TTerm (
        -- * Annotations
    Annotation(..),

        -- * Terms
    TermF(..), DTermV, DVar, DFunct, DFunctAr, DTerm,

        -- * Rules
    DAgg, DRule(..),

        -- * Convenience re-export
    UTerm(..)
) where

import           Control.Unification
import qualified Data.ByteString     as B
import qualified Data.Foldable       as F
import qualified Data.Traversable    as T

------------------------------------------------------------------------}}}
-- Terms                                                                {{{

data Annotation t = AnnType t
 deriving (Eq,F.Foldable,Functor,Ord,Show,T.Traversable)

data TermF a t = TFunctor !a ![t]
               | TNumeric !(Either Integer Double)
               | TString  !B.ByteString
 deriving (Eq,F.Foldable,Functor,Ord,Show,T.Traversable)

type DFunct = B.ByteString
type DFunctAr = (DFunct,Int)
type DTermV v = UTerm (TermF DFunct) v

type DVar  = B.ByteString
type DTerm = DTermV DVar

------------------------------------------------------------------------}}}
-- Instances                                                            {{{

instance (Eq a) => Unifiable (TermF a) where
  zipMatch (TFunctor a as) (TFunctor b bs) | a == b
                                             && length as == length bs
     = Just (TFunctor a (zipWith (\aa ba -> Right (aa,ba)) as bs))
  zipMatch _ _                                      = Nothing

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

type DAgg = B.ByteString

data DRule = Rule !DTerm !DAgg ![DTerm] !DTerm
 deriving (Show)

------------------------------------------------------------------------}}}
