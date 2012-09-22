---------------------------------------------------------------------------
-- Header material
------------------------------------------------------------------------{{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

  -- | An AST for K3.
  --
  -- To as large of an extent as possible, we wish to capture the static 
  -- semantics of K3 in the Haskell type system.
module Dyna.BackendK3.AST where

import           Data.Word
import           GHC.Prim (Constraint)

------------------------------------------------------------------------}}}
-- Preliminaries
------------------------------------------------------------------------{{{

newtype VarIx  = Var String
newtype AddrIx = Addr (String,Int)

data Ann = Ann [String]

------------------------------------------------------------------------}}}
-- Collections
------------------------------------------------------------------------{{{

data CKind = CSet | CBag | CList

data CTE (c :: CKind) e

data CollTy c where
  CTSet  :: CollTy CSet
  CTBag  :: CollTy CBag
  CTList :: CollTy CList

------------------------------------------------------------------------}}}
-- Effectables (XXX TODO)
------------------------------------------------------------------------{{{

{-
data MKind = MKImmut | MKMut

data MTy m where
  MTImmut :: MTy MKImmut
  MTMut   :: MTy MKMut

data VKind = VKIsol | VKCont

data VTy v where
  VTIsol :: VTy VKIsol
  VTCont :: VTy VKCont
-}

------------------------------------------------------------------------}}}
-- Type System
------------------------------------------------------------------------{{{

  -- | Data level representation of K3 types, indexed by equivalent type in
  -- Haskell.
class K3Ty (r :: * -> *) where
    -- | Attach an annotation to a type
  tAnn    :: Ann -> r a -> r a

  tBool   :: r Bool
  tByte   :: r Word8
  tFloat  :: r Float
  tInt    :: r Int
  tString :: r String
  tUnit   :: r ()
  tUnk    :: r a

{- TAddress | TTarget BaseTy -}

  tPair   :: r a -> r b -> r (a,b)

  tMaybe  :: r a -> r (Maybe a)

  tColl   :: CollTy c -> r a -> r (CTE c a)

  tFun    :: r a -> r b -> r (a -> b)

  -- | Existential typeclass wrapper for K3Ty
newtype ExTyRepr (a :: *) = ETR { unETR :: forall r . (K3Ty r) => r a }
instance K3Ty ExTyRepr where
  tAnn   s (ETR t)               = ETR$tAnn s t
  tBool                          = ETR tBool
  tByte                          = ETR tByte
  tFloat                         = ETR tFloat
  tInt                           = ETR tInt
  tString                        = ETR tString
  tUnit                          = ETR tUnit
  tUnk                           = ETR tUnk

  tPair  (ETR a) (ETR b) = ETR$tPair a b
  tMaybe (ETR a)         = ETR$tMaybe a
  tColl  c (ETR a)       = ETR$tColl c a
  tFun   (ETR a) (ETR b) = ETR$tFun a b

------------------------------------------------------------------------}}}
-- Pattern System
------------------------------------------------------------------------{{{

  -- | Kinds of patterns permitted in K3
data PKind where
  PKVar  :: k -> PKind
  PKJust :: PKind -> PKind
  PKPair :: PKind -> PKind -> PKind

  -- | Provides witnesses that certain types may be used
  --   as arguments to K3 lambdas.  Useful when building
  --   up type signatures and pattern matches in lambdas.
  --
  --   Note that this is a closed class using the promoted
  --   data PKind.
class Pat (w :: PKind) where
    -- | Any data this witness needs to carry around
  data PatDa w :: *
    -- | The type this witness witnesses?
  type PatTy w :: *
    -- | The type of this pattern.
  type PatReprFn w (r :: * -> *) :: *
    -- | Produce a data-level type representation for this witness
  patAsRepr :: PatDa w -> ExTyRepr (PatTy w)

instance Pat (PKVar (a :: *)) where
  -- | Pattern variables may be of any type, but we have to
  --   have a representation builder for it.
  data PatDa (PKVar a) = PVar { unPVar :: ExTyRepr a }
  type PatTy (PKVar a) = a
  type PatReprFn (PKVar a) r = r a

  patAsRepr = unPVar

instance (Pat w) => Pat (PKJust w) where
  -- | Just patterns (fail on Nothing)
  --
  -- Note the distinction between PatTy and PatReprFn here!
  -- This pattern witnesses a type "Maybe a" but binds a variable of type
  -- "a".  This will in general be true of any variant (i.e. sum) pattern.
  data PatDa (PKJust w)       = PJust (PatDa w)
  type PatTy (PKJust w)       = Maybe (PatTy w)
  type PatReprFn (PKJust w) r = PatReprFn w r

  patAsRepr (PJust w') = ETR $ tMaybe $ unETR $ patAsRepr w'

instance (Pat wa, Pat wb) => Pat (PKPair wa wb) where
  -- | Pair patterns
  --
  -- Product patterns, on the other hand, have PatTy and PatReprFn both
  -- producing tuples.
  data PatDa (PKPair wa wb)       = PPair (PatDa wa) (PatDa wb)
  type PatTy (PKPair wa wb)       = (PatTy wa, PatTy wb)
  type PatReprFn (PKPair wa wb) r = (PatReprFn wa r, PatReprFn wb r)

  patAsRepr (PPair wa wb) = ETR $ tPair (unETR $ patAsRepr wa)
                                        (unETR $ patAsRepr wb)

------------------------------------------------------------------------}}}
-- Slice System
------------------------------------------------------------------------{{{

  -- | Kinds of slices permitted in K3
data SKind where
  SKVar  :: k -> SKind
  SKJust :: SKind -> SKind
  SKPair :: SKind -> SKind -> SKind

  -- | Witness of slice well-formedness
class Slice (w :: SKind) where
  data SliceDa w :: *
  type SliceTy w :: *
  sliceAsRepr :: SliceDa w -> ExTyRepr (SliceTy w)

  -- Slice variables are VarIx and representation of K3 type
instance Slice (SKVar (a :: *)) where
  data SliceDa (SKVar a) = SVar VarIx (ExTyRepr a)
  type SliceTy (SKVar a) = a
  sliceAsRepr (SVar _ ea) = ea

instance (Slice s) => Slice (SKJust s) where
  data SliceDa (SKJust s) = SJust (SliceDa s)
  type SliceTy (SKJust s) = Maybe (SliceTy s)
  sliceAsRepr (SJust a) = ETR $ tMaybe $ unETR $ sliceAsRepr a

instance (Slice sa, Slice sb) => Slice (SKPair sa sb) where
  data SliceDa (SKPair sa sb) = SPair (SliceDa sa) (SliceDa sb)
  type SliceTy (SKPair sa sb) = (SliceTy sa, SliceTy sb)
  sliceAsRepr (SPair a b) = ETR $ tPair (unETR $ sliceAsRepr a)
                                        (unETR $ sliceAsRepr b)

------------------------------------------------------------------------}}}
-- Numeric Autocasting
------------------------------------------------------------------------{{{

  -- | Unary numerics
class UnNum a where unneg :: a -> a
instance UnNum Bool  where unneg = not
instance UnNum Int   where unneg x = (-x)
instance UnNum Float where unneg x = (-x)

  -- | Binary numerics
class BiNum a b where 
  type BNTF a b :: *
  biadd :: a -> b -> BNTF a b
  bimul :: a -> b -> BNTF a b

instance BiNum Bool Bool where 
  type BNTF Bool Bool = Bool
  biadd = (||)
  bimul = (&&)

instance BiNum Int Int where 
  type BNTF Int Int = Int
  biadd = (+)
  bimul = (*)

instance BiNum Float Float where
  type BNTF Float Float = Float
  biadd = (+)
  bimul = (*)

  -- XXX More

------------------------------------------------------------------------}}}
-- Values and Expressions
------------------------------------------------------------------------{{{

class K3 (r :: * -> *) where
    -- | A representation-specific constraint for collections, on functions
    -- which need to dispatch on a type-tag in the output.
  type K3AST_Coll_C r (c :: CKind) :: Constraint

    -- | A representation-specific constraint on handling patterns, on any
    -- function which uses patterns.
  type K3AST_Pat_C r (w :: PKind) :: Constraint

    -- | A representation-specific constraint for slices, on eSlice.
  type K3AST_Slice_C r (w :: SKind) :: Constraint

    -- | Add a comment to some part of the AST
  cComment  :: String -> r a -> r a
    -- | Add some annotations to some part of the AST
  cAnn      :: Ann -> r a -> r a

    -- XXX An escape hatch
  cUnk      :: r a 

    -- XXX cAddress  :: AddrIx -> r AddrIx
  cBool     :: Bool -> r Bool
  cByte     :: Word8 -> r Word8
  cFloat    :: Float -> r Float
  cInt      :: Int -> r Int
  cNothing  :: r (Maybe a)
  cString   :: String -> r String
  cUnit     :: r ()

    -- XXX polymorphic type because the expression might be
    -- well-formed; we'll have to resolve it later.
  eVar      :: VarIx -> r a

  ePair     :: r a -> r b -> r (a,b)
  eJust     :: r a -> r (Maybe t)

  eEmpty    :: (K3AST_Coll_C r c) => r (CTE c e)
  eSing     :: (K3AST_Coll_C r c) => r e -> r (CTE c e)
  eComb     :: r (CTE c e) -> r (CTE c e) -> r (CTE c e)
  eRange    :: r Int -> r Int -> r Int -> r (CTE c Int)

  eAdd      :: (BiNum a b) => r a -> r b -> r (BNTF a b)
  eMul      :: (BiNum a b) => r a -> r b -> r (BNTF a b)
  eNeg      :: (UnNum a)   => r a -> r a 

    -- XXX Constraints?
  eEq       :: r a -> r a -> r Bool
  eLt       :: r a -> r a -> r Bool
  eLeq      :: r a -> r a -> r Bool
  eNeq      :: r a -> r a -> r Bool

    -- Unlike traditional lambdas, we require a witness
    -- that the argument is admissible in K3.
  eLam      :: (K3AST_Pat_C r w, Pat w)
            => PatDa w -> (PatReprFn w r -> r b) -> r (PatTy w -> b)
  eApp      :: r (a -> b) -> r a -> r b

  eBlock    :: [r ()] -> r a -> r a

  eIter     :: r (t -> ()) -> r (CTE c t) -> r ()

  eITE      :: r Bool -> r a -> r a -> r a

  eMap      :: r (t -> t') -> r (CTE c t) -> r (CTE c t')
  eFiltMap  :: r (t -> Bool) -> r (t -> t') -> r (CTE c t) -> r (CTE c t')

  eFlatten  :: r (CTE c (CTE c' t)) -> r (CTE c' t)

    -- | Called Aggregate in K3's AST
  eFold     :: r ((t', t) -> t') -> r t' -> r (CTE c t) -> r t'
  eGBA      :: r (t -> t'') -> r ((t',t) -> t') -> r t' -> r (CTE c t) -> r (CTE c (t'',t'))

  eSort     :: r (CTE c t) -> r ((t,t) -> Bool) -> r (CTE 'CList t)

  ePeek     :: r (CTE c e) -> r e

    -- | Slice out from a collection; the slice's type and
    -- the type of elements of the collection must match.
    --
    -- Rather like lambdas, except that the witness is also
    -- a mandatory part of the definition of "slice" :)
  eSlice    :: (K3AST_Slice_C r w, Slice w, SliceTy w ~ t)
            => SliceDa w -> r (CTE c t) -> r (CTE c t)

  eInsert   :: r (CTE c t) -> r t -> r ()
  eDelete   :: r (CTE c t) -> r t -> r ()
  eUpdate   :: r (CTE c t) -> r t -> r t -> r ()

  -- XXX eAssign
  -- XXX eDeref
  -- XXX eSend

------------------------------------------------------------------------}}}
-- Miscellanious
------------------------------------------------------------------------{{{

  -- XXX does not enumerate local variables
data Decl tr r t = Decl VarIx (tr t) (Maybe (r t))

  -- | A convenience function for setting the type of a collection.
  --
  -- Use as (eEmpty `asColl` CTSet)
asColl :: r (CTE c t) -> CollTy c -> r (CTE c t)
asColl = const

------------------------------------------------------------------------}}}
-- fin
---------------------------------------------------------------------------
