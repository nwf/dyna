---------------------------------------------------------------------------
-- | HLists using the GHC machinery new in 7.6.

-- Header material                                                      {{{

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.XXX.HList(
  -- * Type-level functions on lifted lists
  Append, Map, MapConstraint,

  -- * Data-level HLists
  HList(..), hlmapa,

  -- * HLists of a type constructor's images
  HRList(..), hrlmapa, hrllen, hrlmap, hrlproj, hrlTravProj,

  -- * Interoperation between HList and HRList
  HLR(..), HLRFunc(..), hlToHrl,
) where

import           Prelude (Eq(..),Ord(..),Show(..),
                          Bool(..),(&&),
                          Ordering(..),
                          Int,(+),
                          (++),
                          seq)
import           Control.Applicative (Applicative,pure,liftA2)
import           GHC.Prim (Constraint)
import qualified GHC.TypeLits as TL

------------------------------------------------------------------------}}}
-- Rebind List Syntax                                                   {{{

{-
infixr 5 :

class ListOp a e where
  ([]) :: a
  (:)  :: e -> a -> a
-}

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

-- | Inverse of 'Map'
type family UnMap (f :: k -> k') (x :: [k']) :: [k]
type instance UnMap f '[] = '[]
type instance UnMap f (f a ': fas) = a ': UnMap f fas

{-
-- | Filter
type family Filter (f :: k -> Bool) (xs :: [k]) :: [k']
type instance Filter f '[] = '[]
type instance Filter f (x ': xs) = MaybeCons (f x) x (Filter f xs)

type family MaybeCons (mk :: Bool) (x :: k) (xs :: [k]) :: [k]
type instance MaybeCons False x xs = xs
type instance MaybeCons True  x xs = x ': xs
-}

type family Length (x :: [k]) :: TL.Nat
type instance Length '[] = 0
type instance Length (x ': xs) = 1 TL.+ (Length xs)

-- | Apply a type function yielding a constraint to each element of a
-- lifted list.
type family MapConstraint (f :: k -> Constraint) (x :: [k]) :: Constraint
type instance MapConstraint f '[] = ()
type instance MapConstraint f (x ': xs) = (f x, MapConstraint f xs)

------------------------------------------------------------------------}}}
-- Classes                                                              {{{

-- | Assert that two lifted-lists are of the same length
class EqLen (x :: [k]) (y :: [k'])
instance EqLen '[] '[]
instance (EqLen xs ys) => EqLen (x ': xs) (y ': ys)

------------------------------------------------------------------------}}}
-- Data-level HLists                                                    {{{

infixr 5 :+

-- | A heterogeneous list
data HList a where
 HN   :: HList '[]
 (:+) :: a -> HList b -> HList (a ': b)

instance Show (HList '[]) where show _ = "HN"
instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
    show (a :+ as) = show a ++ " :+ " ++ show as

instance Eq (HList '[]) where _ == _ = True
instance (Eq a, Eq (HList as)) => Eq (HList (a ': as)) where
    (a :+ as) == (b :+ bs) = a == b && as == bs

instance Ord (HList '[]) where compare _ _ = EQ
instance (Ord a, Ord (HList as)) => Ord (HList (a ': as)) where
    compare (a :+ as) (b :+ bs) = case compare a b of
        EQ -> compare as bs
        x  -> x

hlmapa :: (Applicative m, Map c t ~ t')
       => (forall a . a -> m (c a)) -> HList t -> m (HList t')
hlmapa _ HN = pure HN
hlmapa f (a :+ as) = liftA2 (:+) (f a) (hlmapa f as)

{-
 - XXX Why can't I make this work?
hrlmap' :: (EqLen ras sas, HLR r as ras, HLR s as sas)
        => (forall a . r a -> s a) -> HList ras -> HList sas
hrlmap' _  HN       = HN
hrlmap' f (a :+ as) = f a :+ (hrlmap' f as)
-}


------------------------------------------------------------------------}}}
-- HRLists - HLists of a type constructor's images                      {{{

infixr 5 :++

-- | A heterogeneous list in which every element is the image of some
-- type-function @r :: * -> *@.
data HRList (r :: k -> *) (a :: [k]) where
 HRN   :: HRList r '[]
 (:++) :: r a -> HRList r b -> HRList r (a ': b)

instance Show (HRList r '[]) where show _ = "HRN"
instance (Show (r a), Show (HRList r as)) => Show (HRList r (a ': as)) where
    show (a :++ as) = show a ++ " :++ " ++ show as

hrllen :: HRList r t -> Int
hrllen = go 0
 where
  go :: Int -> HRList r t -> Int
  go a HRN = a
  go a (x :++ xs) = a `seq` go (a+1) xs

-- | Shift the type-function of a HRList
hrlmap :: (forall a . r a -> s a) -> HRList r t -> HRList s t
hrlmap _  HRN       = HRN
hrlmap f (a :++ as) = f a :++ (hrlmap f as)

hrlmapa :: (Applicative m)
        => (forall a . r a -> m (s a)) -> HRList r t -> m (HRList s t)
hrlmapa _  HRN       = pure HRN
hrlmapa f (a :++ as) = liftA2 (:++) (f a) (hrlmapa f as)

-- | Eliminate a HRList to a homogeneous list
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

class (Map r a ~ ra, UnMap r ra ~ a) => HLR (r :: k -> k') (a :: [k]) (ra :: [k'])
instance HLR r '[] '[]
instance (ra ~ r a, HLR r as ras) => HLR r (a ': as) (ra ': ras)

-- | Kind-restricted version of HLR that offers functionality for
-- manipulating data.
class (HLR r a ra)
   => HLRFunc (r :: * -> *) (a :: [*]) (ra :: [*])
 where
  -- | Reinterpret an HList as an HRList
  hlAsHrl :: HList ra -> HRList r a
  -- | Reinterpret an HRList as an HList
  hrlAsHl :: HRList r a -> HList ra
  -- | Derive an HList by unwrapping each element.
  hrlToHl :: (forall b . r b -> b) -> HRList r a -> HList a
instance HLRFunc r '[] '[]
 where
  hlAsHrl _ = HRN
  hrlAsHl _ = HN
  hrlToHl _ _ = HN
instance (ra ~ r a, HLRFunc r as ras) => HLRFunc r (a ': as) (ra ': ras)
 where
  hlAsHrl (a :+ as) = a :++ hlAsHrl as
  hrlAsHl (a :++ as) = a :+ hrlAsHl as
  hrlToHl f (ra :++ ras) = f ra :+ hrlToHl f ras

------------------------------------------------------------------------}}}
