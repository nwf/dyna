---------------------------------------------------------------------------
--  | HLists using the GHC machinery new in 7.6.

-- Header material                                                      {{{

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Dyna.XXX.HList(
  HList(..), MapStarConst, hlToHrl, HRList(..), hrlmap, hrlproj
) where

import           GHC.Prim (Constraint)

------------------------------------------------------------------------}}}
-- Data-level HLists                                                    {{{

infixr 5 :+

-- | A heterogenous list
data HList a where
 HN   :: HList '[]
 (:+) :: a -> HList b -> HList (a ': b)

instance Show (HList '[]) where show _ = "HN"
instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
    show (a :+ as) = show a ++ " :+ " ++ show as

hlToHrl :: (forall b . b -> r b) -> HList a -> HRList r a
hlToHrl _ HN = HRN
hlToHrl f (a:+as) = f a :++ (hlToHrl f as)

type family MapStarConst (f :: * -> Constraint) (x :: [*]) :: Constraint
type instance MapStarConst f '[] = ()
type instance MapStarConst f (x ': xs) = (f x, MapStarConst f xs)

infixr 5 :++

-- | A heterogenous list in which every element is the image of some
-- type-function @r :: * -> *@.
data HRList r a where
 HRN   :: HRList r '[]
 (:++) :: r a -> HRList r b -> HRList r (a ': b)

instance Show (HRList r '[]) where show _ = "HRN"
instance (Show (r a), Show (HRList r as)) => Show (HRList r (a ': as)) where
    show (a :++ as) = show a ++ " :+ " ++ show as

-- | Shift the type-function of a HRList
hrlmap :: (forall a . r a -> s a) -> HRList r t -> HRList s t
hrlmap _  HRN       = HRN
hrlmap f (a :++ as) = f a :++ (hrlmap f as)

-- | Eliminate a HRList to a homogenous list
hrlproj :: (forall a . r a -> b) -> HRList r t -> [b]
hrlproj _  HRN       = []
hrlproj f (a :++ as) = f a : (hrlproj f as)

------------------------------------------------------------------------}}}
