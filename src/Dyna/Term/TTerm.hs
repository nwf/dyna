---------------------------------------------------------------------------
-- | Very, very basic (Trivial, even) representation of terms.
--
-- XXX This isn't going to be sufficient when we start doing more
-- complicated things, but it suffices for now?

-- Header material                                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
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

        -- * Term Base Cases
    TBase(..), TBaseSkolem(..),

        -- * Terms
    TermF(..), {- DTermV, -} DVar, DFunct, DFunctAr, {- DTerm, -}

        -- * Rules
    DAgg, {- DRule(..), -}

        -- * Convenience re-export
    -- UTerm(..)
) where

import qualified Data.ByteString       as B
import qualified Data.Data             as D
import qualified Data.Foldable         as F
import qualified Data.Traversable      as T
import qualified Text.PrettyPrint.Free as PP

------------------------------------------------------------------------}}}
-- Term Base Cases                                                      {{{

data TBaseSkolem = TSNumeric | TSString
 deriving (Eq,Ord,Show)

-- | Term base cases.
data TBase = TNumeric !(Either Integer Double)
           | TString  !B.ByteString
 deriving (D.Data,D.Typeable,Eq,Ord,Show)

instance PP.Pretty TBase where
    pretty (TNumeric (Left x))  = PP.pretty x
    pretty (TNumeric (Right x)) = PP.pretty x
    pretty (TString s)          = PP.dquotes (PP.pretty s)

------------------------------------------------------------------------}}}
-- Terms                                                                {{{

data Annotation t = AnnType t
 deriving (D.Data,D.Typeable,Eq,F.Foldable,Functor,Ord,Show,T.Traversable)

data TermF a t = TFunctor !a ![t]
               | TBase !TBase
 deriving (Eq,F.Foldable,Functor,Ord,Show,T.Traversable)

type DFunct = B.ByteString
type DFunctAr = (DFunct,Int)

type DVar  = B.ByteString

------------------------------------------------------------------------}}}
-- Instances                                                            {{{

{-
instance (Eq a) => Unifiable (TermF a) where
  zipMatch (TFunctor a as) (TFunctor b bs) | a == b
                                             && length as == length bs
     = Just (TFunctor a (zipWith (\aa ba -> Right (aa,ba)) as bs))
  zipMatch _ _                                      = Nothing
-}

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

type DAgg = B.ByteString

{-
data DRule = Rule !DTerm !DAgg ![DTerm] !DTerm
 deriving (Show)
-}

------------------------------------------------------------------------}}}
