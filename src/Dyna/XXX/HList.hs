---------------------------------------------------------------------------
-- | HLists using the GHC machinery new in 7.6.

-- Header material                                                      {{{

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.XXX.HList(
  -- * Proxy
  Proxy(..),
  -- * Type-level functions on lifted lists
  Append, Map, MapConstraint,

  -- * Data-level HLists
  HList(..), hlmapa,

  -- * HLists of a type constructor's images
  HRList(..), hrlmapa, hrlmap, hrlproj, hrlTravProj,

  -- * Interoperation between HList and HRList
  HLR(..), hlToHrl, HRL2HL
) where

import           Control.Applicative
import           GHC.Prim (Constraint)

------------------------------------------------------------------------}}}
-- Proxy                                                                {{{

-- | Capture a type-constructor as data.
--
-- /Good luck!  I am behind seven/.
data Proxy (r :: k) = Proxy

------------------------------------------------------------------------}}}
-- Type-level functions                                                 {{{

-- | Compute the type of appending two lifted lists.
type family Append (x :: [k]) (y :: [k]) :: [k]
type instance Append '[] y = y
type instance Append (x ': xs) y = x ': Append xs y

-- | Compute the type of an HList subject to some type function
type family Map (f :: k -> k') (x :: [k]) :: [k']
type instance Map f '[] = '[]
type instance Map f (a ': as) = (f a) ': Map f as

-- | Apply a type function yielding a constraint to each element of a
-- lifted list.
type family MapConstraint (f :: k -> Constraint) (x :: [k]) :: Constraint
type instance MapConstraint f '[] = ()
type instance MapConstraint f (x ': xs) = (f x, MapConstraint f xs)

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

hlmapa :: (Applicative m, Map c t ~ t')
       => (forall a . a -> m (c a)) -> HList t -> m (HList t')
hlmapa _ HN = pure HN
hlmapa f (a :+ as) = liftA2 (:+) (f a) (hlmapa f as)

------------------------------------------------------------------------}}}
-- HRLists - HLists of a type constructor's images                      {{{

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

hrlmapa :: (Applicative m)
        => (forall a . r a -> m (s a)) -> HRList r t -> m (HRList s t)
hrlmapa _  HRN       = pure HRN
hrlmapa f (a :++ as) = liftA2 (:++) (f a) (hrlmapa f as)

-- | Eliminate a HRList to a homogenous list
hrlproj :: (forall a . r a -> b) -> HRList r t -> [b]
hrlproj _  HRN       = []
hrlproj f (a :++ as) = f a : (hrlproj f as)

hrlTravProj :: (Applicative f) => (forall a . r a -> f b) -> HRList r t -> f [b]
hrlTravProj _ HRN = pure []
hrlTravProj f (a :++ as) = liftA2 (:) (f a) (hrlTravProj f as)

------------------------------------------------------------------------}}}
-- Interoperation between HList and HRList                              {{{

-- | Produce a HRList from an HList by wrapping each element
hlToHrl :: (forall b . b -> r b) -> HList a -> HRList r a
hlToHrl _ HN = HRN
hlToHrl f (a:+as) = f a :++ (hlToHrl f as)

type family HRL2HL (r :: k -> k') (a :: [k]) :: [k']
type instance HRL2HL r '[] = '[]
type instance HRL2HL r (a ': as) = r a ': (HRL2HL r as)

class (HRL2HL r a ~ ra)
   => HLR (r :: * -> *) (a :: [*]) (ra :: [*]) | ra -> r a, r a -> ra
 where
  -- | Reinterpret an HList as an HRList
  hlAsHrl :: HList ra -> HRList r a
  -- | Reinterpret an HRList as an HList
  hrlAsHl :: HRList r a -> HList ra
  -- | Derive an HList by unwrapping each element.
  hrlToHl :: (forall b . r b -> b) -> HRList r a -> HList a
instance HLR r '[] '[]
 where
  hlAsHrl _ = HRN
  hrlAsHl _ = HN
  hrlToHl _ _ = HN
instance (ra ~ r a, HLR r as ras) => HLR r (a ': as) (ra ': ras)
 where
  hlAsHrl (a :+ as) = a :++ hlAsHrl as
  hrlAsHl (a :++ as) = a :+ hrlAsHl as
  hrlToHl f (ra :++ ras) = f ra :+ hrlToHl f ras

------------------------------------------------------------------------}}}
