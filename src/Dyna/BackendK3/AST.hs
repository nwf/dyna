---------------------------------------------------------------------------
-- | An AST for K3.
--
-- To as large of an extent as possible, we wish to capture the static 
-- semantics of K3 in the Haskell type system.
--
-- This file uses some rather exciting extensions and as such is in a
-- Haskell-friendly definition order, rather than the cleanest
-- human-friendly ordering.  Sorry about that.
--
-- XXX Currently does not have any mechanism for declaring local variables
-- The K3 Way -- right now the only way to do that is let-as-lambda.
--
-- XXX k3ref K3 does not yet support references.  This string is littered
-- through the codebase from a previous, incomplete attempt at handling
-- references which has been commented out to prevent the overly ambitious
-- from attempting to make use of it.
--
-- XXX k3xref K3 does not yet support foreign key constraints.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Dyna.BackendK3.AST (
    -- * Preliminaries
    VarIx(..), AddrIx(..), Target,

    -- * Numeric Autocasting
    BiNum(..), UnNum(..),

    -- * Collections
    CTE, CKind(..), CollTy(..), asColl,

{- XXX k3ref
    -- * References
    Ref, 
-}

    -- * Pattern System
    PKind(..), Pat(..), PDat(..),
    MapPatConst,

    -- * Annotations
    AnnT(..), AnnE(..), FunDepSpec(..),

    -- * Type System: Base constraints
    K3BaseTy,

    -- * Type System: Representations
    K3Ty(..), UnivTyRepr(..),

    -- * Expressions
    K3(..), 

{- XXX
    -- * Roles and Streams
    -- SFormat(..), Stream(..), K3RoleDesc(..),
-}

    -- * Declarations
    Decl(..), DBody(..), asR, asCollR, {- asRefR, -}
    mkCollDecl, mkTrigDecl, -- unUDR,

    -- * Programs
    mkK3, mkK3T, MkK3T, Prog(..)
) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Proxy
import           Data.Word
import           GHC.Prim (Constraint)
-- import           Language.Haskell.TH (varT, mkName)

import           Dyna.XXX.HList
import           Dyna.XXX.MonadUtils
import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

  -- XXX
newtype VarIx  = Var String
 deriving (Eq,Ord,Show)

  -- XXX (Hostname,Port)
newtype AddrIx = Addr (String,Int)
 deriving (Eq,Show)

------------------------------------------------------------------------}}}
-- Type System: Base Constraints                                        {{{

-- | A constraint for /base/ types in K3.  These are the things that can
-- be passed to lambdas.  Essentially everything other than arrows.
class    K3BaseTy a
instance K3BaseTy Bool
instance K3BaseTy Word8
instance K3BaseTy Float
instance K3BaseTy Int
instance K3BaseTy String
instance K3BaseTy ()
instance (K3BaseTy a) => K3BaseTy (Maybe a)
instance K3BaseTy (HList '[])
instance (K3BaseTy a, K3BaseTy (HList as)) => K3BaseTy (HList (a ': as))
$(mkTupleRecInstances ''K3BaseTy [])

------------------------------------------------------------------------}}}
-- Type System: Proxified Representations                               {{{

type family K3Proxied (a :: *) :: *
type family K3Unproxy (r :: * -> *) (a :: *) :: *

type instance K3Proxied   Bool   = Bool
type instance K3Unproxy r Bool   = Bool

type instance K3Proxied   Word8  = Word8
type instance K3Unproxy r Word8  = Word8

type instance K3Proxied   Float  = Float
type instance K3Unproxy r Float  = Float

type instance K3Proxied   Int    = Int
type instance K3Unproxy r Int    = Int

type instance K3Proxied   String = String
type instance K3Unproxy r String = String

type instance K3Proxied   ()     = ()
type instance K3Unproxy r ()     = ()

type instance K3Proxied   (Maybe a) = Maybe (K3Proxied   a)
type instance K3Unproxy r (Maybe a) = Maybe (K3Unproxy r a)

type family MapK3Proxied (a :: [*]) :: [*]
type instance MapK3Proxied '[] = '[]
type instance MapK3Proxied (a ': as) = K3Proxied a ': (MapK3Proxied as)

type family MapK3Unproxy (r :: * -> *) (a :: [*]) :: [*]
type instance MapK3Unproxy r '[] = '[]
type instance MapK3Unproxy r (a ': as) = K3Unproxy r a ': (MapK3Unproxy r as)

type instance K3Proxied   (HList a) = HList (MapK3Proxied   a)
type instance K3Unproxy r (HList a) = HList (MapK3Unproxy r a)

type instance K3Proxied   (a,b) = (K3Proxied   a, K3Proxied   b)
type instance K3Unproxy r (a,b) = (K3Unproxy r a, K3Unproxy r b)

type instance K3Proxied   (a,b,c) = (K3Proxied   a, K3Proxied   b,
                                     K3Proxied   c)
type instance K3Unproxy r (a,b,c) = (K3Unproxy r a, K3Unproxy r b,
                                     K3Unproxy r c)

type instance K3Proxied   (a,b,c,d) = (K3Proxied   a, K3Proxied   b,
                                       K3Proxied   c, K3Proxied   d)
type instance K3Unproxy r (a,b,c,d) = (K3Unproxy r a, K3Unproxy r b,
                                       K3Unproxy r c, K3Unproxy r d)

type instance K3Proxied   (a,b,c,d,e) = (K3Proxied   a, K3Proxied   b,
                                         K3Proxied   c, K3Proxied   d,
                                         K3Proxied   e)
type instance K3Unproxy r (a,b,c,d,e) = (K3Unproxy r a, K3Unproxy r b,
                                         K3Unproxy r c, K3Unproxy r d,
                                         K3Unproxy r e)

------------------------------------------------------------------------}}}
-- Targets                                                              {{{

-- | The 'r' representation of a target taking argument type 't'.
--
-- This is similar to @t -> ()@ except that it executes in a different
-- transaction and must be named.  The only safe source of Targets is a
-- 'Decl', but being first class, they can be stored in collections for
-- dynamic dispatch.
data family Target (r :: * -> *) t :: *

instance (K3BaseTy a) => K3BaseTy (Target r a)

type instance K3Proxied   (Target r     a) = Target Proxy (K3Proxied   a)
type instance K3Unproxy r (Target Proxy a) = Target r     (K3Unproxy r a)

------------------------------------------------------------------------}}}
-- Collections                                                          {{{

-- | Reflect 'CollTy' at the type level.
data CKind = CKBag | CKList | CKSet

-- | The 'r' representation of a collection of kind 'c' of elements 'e'.
data family CTE (r :: * -> *) (c :: CKind) e

-- | Data-level specification of collection kinds
data CollTy c where
  -- | Bags are unordered collections of elements which may have duplicates.
  CBag  :: CollTy CKBag
  -- | Lists are linearly ordered collecitons of elements in the usual
  -- mu-recursive style.
  CList :: CollTy CKList
  -- | Sets are unordered collections of elements with no duplicates.
  CSet  :: CollTy CKSet

instance (K3BaseTy a) => K3BaseTy (CTE r c a)

type instance K3Proxied   (CTE r     c a) = CTE Proxy c (K3Proxied   a)
type instance K3Unproxy r (CTE Proxy c a) = CTE r     c (K3Unproxy r a)

-- | A convenience function for setting the type of a collection.
--
-- Use as (eEmpty `asColl` CTSet)
asColl :: r (CTE r c t) -> CollTy c -> r (CTE r c t)
asColl = const

------------------------------------------------------------------------}}}
-- References                                                           {{{

{- XXX k3ref
-- | The 'r' representation of references of elements of type 'a'
data family Ref (r :: * -> *) a

instance (K3BaseTy a) => K3BaseTy (Ref r a)

type instance K3Proxied   (Ref r     a) = Ref Proxy (K3Proxied   a)
type instance K3Unproxy r (Ref Proxy a) = Ref r     (K3Unproxy r a)
 -}

------------------------------------------------------------------------}}}
-- Pattern System                                                       {{{

-- | Kinds of patterns permitted in K3
data PKind where
  -- | Variables in patterns (see 'PVar')
  PKVar  :: r -> k -> PKind

  -- | Wildcards in patterns (see 'PUnk')
  PKUnk  :: k -> PKind

  -- | Just patterns (see 'PJust')
  PKJust :: PKind -> PKind

{- XXX k3ref
  -- | Ref patterns (see 'PRef')
  PKRef :: PKind -> PKind
 -}

  -- | HList patterns
  PKHL :: [PKind] -> PKind

  -- | Product ("tuple") patterns
  --
  -- 'PatTy' and 'PatReprFn' both produce tuples.
  PKTup  :: [PKind] -> PKind

-- | Data-representation of patterns in K3.
--
--   The 'r' parameter is the representation of components of the pattern
--   itself, and is used to constrain representation of variables.  When
--   being used as slices, @r@ will be the same as @s@ given to the type
--   functions in the 'Pat' class below; when being used as patterns for
--   lambdas, @r ~ UnivTyRepr@ and 's' is a 'K3'.  In this latter case,
--   the 'UnivTyRepr' is needed for "Dyna.BackendK3.Render"'s function,
--   since every lambda needs an explicit type signature on its pattern.
data PDat (r :: * -> *) (k :: PKind) where
  -- | A variable used literally in a pattern
  PVar  :: r a -> PDat r (PKVar r (a :: *))

  -- | A pattern wildcard.
  --
  -- Note that 'PatReprFn s (PUnk a) ~ ()', which should prohibit even
  -- accidental use as part of a 'K3' expression or type.
  PUnk  :: PDat r (PKUnk (a :: *))

  -- | A /Just/ pattern.  Eliminates a Just constructor, and causes the
  -- trigger to abort on a Nothing.
  --
  -- Note the distinction between PatTy and (PatBTy and PatReprFn) here!
  -- This pattern witnesses a type "Maybe a" but binds a variable of type
  -- "a".  This will in general be true of any variant (i.e. sum) pattern.
  PJust :: PDat r w -> PDat r (PKJust w)

{- XXX k3ref
  -- | A /Ref/ pattern; dereferences the provided reference.
  PRef  :: PDat r w -> PDat r (PKRef w)
-}

  -- | A HList-style product pattern
  PHL   :: HRList (PDat r) ws -> PDat r (PKHL ws)

  PT2   :: (PDat r w1, PDat r w2) -> PDat r (PKTup [w1,w2])
  PT3   :: (PDat r w1, PDat r w2, PDat r w3) -> PDat r (PKTup [w1,w2,w3])
  PT4   :: (PDat r w1, PDat r w2, PDat r w3, PDat r w4)
        -> PDat r (PKTup [w1,w2,w3,w4])
  PT5   :: (PDat r w1, PDat r w2, PDat r w3, PDat r w4, PDat r w5)
        -> PDat r (PKTup [w1,w2,w3,w4,w5])


-- | Provides type functions on patterns.  Useful when building up type
--   signatures and pattern matches in lambdas.
--
--   Note that this is a closed class using the promoted
--   data 'PKind'.
--
--   Essentially, these things determine where 's's end up
--   in the lambda given to eLam.  Compare and contrast:
--   
-- > eLam (PVar $ tMaybe tInt)            :: (s (Maybe Int)  -> _) -> _
-- > eLam (PJust $ PVar tInt)             :: (s Int          -> _) -> _
--
-- > eLam (PVar $ tPair tInt tInt)        :: (s (Int, Int)   -> _) -> _
-- > eLam (PPair (PVar tInt) (PVar tInt)) :: ((s Int, s Int) -> _) -> _
--
--   The 's' parameter on 'PatTy', 'PatBTy', and 'PatReprFn' is the
--   underlying representation being manipulated, as above.  It is notably
--   used to constrain the representation of 'Ref's to be consistent across
--   a pattern.
--
class Pat (w :: PKind) where
  -- | The type this witness witnesses (i.e. the things matched against)
  type PatTy (s :: * -> *) w :: *
  -- | The type this witness binds (i.e. after matching is done)
  type PatBTy (s :: * -> *) w :: *
  -- | The type of this pattern.
  type PatReprFn (s :: * -> *) w :: *

instance (K3BaseTy a) => Pat (PKVar (r :: * -> *) (a :: *)) where
  type PatTy     s (PKVar r a) =   a
  type PatBTy    s (PKVar r a) =   a
  type PatReprFn s (PKVar r a) = s a

instance (K3BaseTy a) => Pat (PKUnk (a :: *)) where
  type PatTy     s (PKUnk a) =   a
  type PatBTy    s (PKUnk a) =   ()
  type PatReprFn s (PKUnk a) =   ()

instance (Pat w) => Pat (PKJust w) where
  type PatTy     s (PKJust w) = Maybe (PatTy s w)
  type PatBTy    s (PKJust w) = PatBTy s w
  type PatReprFn s (PKJust w) = PatReprFn s w

{- XXX k3ref
instance (Pat w) => Pat (PKRef w) where
  type PatTy     s (PKRef w) = Ref s (PatTy s w)
  type PatBTy    s (PKRef w) = PatBTy s w
  type PatReprFn s (PKRef w) = PatReprFn s w
-}

-- ** Tuples

type family TMapPatTy (s :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatTy ''PatTy)

type family TMapPatBTy (s :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatBTy ''PatBTy)

type family TMapPatReprFn  (r :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatReprFn ''PatReprFn)

type family MapPatConst (x :: [PKind]) :: Constraint
type instance MapPatConst '[] = ()
type instance MapPatConst (x ': xs) = (Pat x, MapPatConst xs)

instance (MapPatConst ts)
      => Pat       (PKTup (ts :: [PKind])) where
  type PatTy     s (PKTup ts) = TMapPatTy s ts
  type PatBTy    s (PKTup ts) = TMapPatBTy s ts
  type PatReprFn s (PKTup ts) = TMapPatReprFn s ts

-- ** HLists

type family MapPatTy (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatTy r ('[]) = '[]
type instance MapPatTy r (w ': ws) = PatTy r w ': (MapPatTy r ws)

type family MapPatBTy (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatBTy r ('[]) = '[]
type instance MapPatBTy r (w ': ws) = PatBTy r w ': (MapPatBTy r ws)

type family MapPatReprFn  (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatReprFn r '[] = '[]
type instance MapPatReprFn r (w ': ws) = PatReprFn r w ': (MapPatReprFn r ws)

instance (MapPatConst ts)
      => Pat     (PKHL (ts :: [PKind])) where
  type PatTy     s (PKHL ts) = HList (MapPatTy s ts)
  type PatBTy    s (PKHL ts) = HList (MapPatBTy s ts)
  type PatReprFn s (PKHL ts) = HList (MapPatReprFn s ts)

------------------------------------------------------------------------}}}
-- Annotations                                                          {{{

-- | Specification for functional dependencies within a collection.
--
-- XXX This has a phantom type only so that we can use it as an r
-- in RTupled.  We'd rather not (see "Dyna.BackendK3.Render"'s
-- need to use 'fdscast'), but the alternative of, e.g. Tagged, is not that
-- great either!
data FunDepSpec a = FDIrr -- ^ /Irr/elevant to a fundep
                  | FDDom -- ^ In the /Dom/ain of a fundep
                  | FDCod -- ^ In the /Cod/omain of a fundep
 deriving (Eq,Show)

-- | Annotations on 'K3Ty' types
data AnnT a where

  -- | A functional dependency among elements of a collection.
  AFunDep :: (RTupled fs, RTR fs ~ FunDepSpec, RTE fs ~ a)
          => fs -> AnnT (CTE r t a)

  -- | Request an additional index on a collection
  AIndex :: (RTupled fs, RTR fs ~ FunDepSpec, RTE fs ~ a)
         => fs -> AnnT (CTE r t a)

  -- | An Exactly-One-Of annotation, used to convey variants (i.e. sums)
  --   to K3.
  AOneOf  :: (RTupled mv, RTR mv ~ Maybe) => AnnT mv

  -- | 'AFunDep' for HList representations
  AFunDepHL :: HRList FunDepSpec a -> AnnT (CTE r t (HList a))

  -- | 'AIndex' for HList representations
  AIndexHL :: HRList FunDepSpec a -> AnnT (CTE r t (HList a))

  -- | 'AOneOf' for HList representations
  AOneOfHL :: (HLR Maybe v mv) => AnnT (HList mv)

{- XXX k3xref
  -- | A cross-reference within this collection
  -- 
  -- Note that the AST supports the use of arbitrary expressions here, which
  -- is not likely the case of the actual K3 system!  Please don't do
  -- something silly.
  --
  -- XXX The need to specify a Proxy is annoying.
  AXref :: (Pat w, PatTy r w ~ t, Pat w', PatTy r w' ~ t)
        => Proxy x
        -> PDat r w  -> (forall p . PatReprFn p w  -> p x)
        -> PDat r w' -> (forall p . PatReprFn p w' -> p x)
        -> AnnT (CTE r c' t)

  -- | A cross-reference to a declared collection.
  --
  -- See the notes for 'AXref'
  AXrefF :: (Pat w, Pat w', PatTy r w ~ t, PatTy r w' ~ t')
         => Proxy x
         -> Decl s (CTE Proxy c' t') -- Foreign collection
         -> PDat UnivTyRepr w' -> (forall p . PatReprFn p w' -> p x)
         -> PDat UnivTyRepr w  -> (forall p . PatReprFn p w  -> p x)
         -> AnnT (CTE r c t)
-}

  -- | An escape hatch! (XXX)
  ATMisc :: String -> AnnT a

	-- XXX Missing: INDEX, UNIQUE, ORDERED, SORTED

-- | Annotations on 'K3' expressions
data AnnE a where

  -- | Decorate an expression as atomic.
  AAtomic :: AnnE a

  -- | Flag that a collection (or collection expression) ought to be a
  -- singleton.  K3's type system is not so deeply embedded into Haskell
  -- that we can check this.
  ASingleton :: AnnE (CTE r t a)

  -- | An escape hatch! (XXX)
  AEMisc :: String -> AnnE a

------------------------------------------------------------------------}}}
-- Type System                                                          {{{

-- | Data level representation of K3 types, indexed by equivalent type in
-- Haskell.
class K3Ty (r :: * -> *) where
  -- | Attach an annotation to a type
  tAnn     :: r a -> [AnnT a] -> r a

  tAddress :: r AddrIx
  tBool    :: r Bool
  tByte    :: r Word8
  tFloat   :: r Float
  tInt     :: r Int
  tString  :: r String
  tTarget  :: r t -> r (Target r' t)
  tUnit    :: r ()

  -- tPair   :: r a -> r b -> r (a,b)
  tMaybe   :: r a -> r (Maybe a)
  -- XXX k3ref
  -- tRef     :: r a -> r (Ref r' a)
  tColl    :: (K3BaseTy a) => CollTy c -> r a -> r (CTE r' c a)
  tFun     :: (K3BaseTy a, K3BaseTy b) => r a -> r b -> r (a -> b)

  -- XXX TUPLES
  -- tTuple  :: (RTupled rt, RTR rt ~ r, RTE rt ~ t) => rt -> r t
  tTuple2  :: (r a, r b) -> r (a,b)
  tTuple3  :: (r a, r b, r c) -> r (a,b,c)
  tTuple4  :: (r a, r b, r c, r d) -> r (a,b,c,d)
  tTuple5  :: (r a, r b, r c, r d, r e) -> r (a,b,c,d,e)

  tHL      :: HRList r a -> r (HList a)

-- | Universal typeclass wrapper for K3Ty
--
-- Often, especially in 'eLam', we need some representation-agnostic proof
-- of type, so we use this.
newtype UnivTyRepr (a :: *) = UTR { unUTR :: forall r . (K3Ty r) => r a }

instance K3Ty UnivTyRepr where
  tAnn   (UTR t) s       = UTR $ tAnn t s

  tAddress               = UTR tAddress
  tBool                  = UTR tBool
  tByte                  = UTR tByte
  tFloat                 = UTR tFloat
  tInt                   = UTR tInt
  tString                = UTR tString
  tTarget (UTR t)        = UTR $ tTarget t
  tUnit                  = UTR tUnit

  tColl   c      (UTR a) = UTR $ tColl c a
  tFun   (UTR a) (UTR b) = UTR $ tFun a b
  tMaybe (UTR a)         = UTR $ tMaybe a
  -- XXX k3ref
  -- tRef   (UTR a)         = UTR $ tRef a

  -- XXX TUPLES
  -- tTuple   us            = UTR $ tTuple  $ tupleopRS unUTR us
  tTuple2  us            = UTR $ tTuple2 $ tupleopRS unUTR us
  tTuple3  us            = UTR $ tTuple3 $ tupleopRS unUTR us
  tTuple4  us            = UTR $ tTuple4 $ tupleopRS unUTR us
  tTuple5  us            = UTR $ tTuple5 $ tupleopRS unUTR us

  tHL      us            = UTR $ tHL     $ hrlmap unUTR us

------------------------------------------------------------------------}}}
-- Numeric Autocasting                                                  {{{

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

-- | And and Or are captured as binary numerics
instance BiNum Bool Bool where 
  type BNTF Bool Bool = Bool
  biadd = (||)
  bimul = (&&)

instance BiNum Int Int where 
  type BNTF Int Int = Int
  biadd = (+)
  bimul = (*)

instance BiNum Int Float where 
  type BNTF Int Float = Float
  biadd a b = ((fromIntegral a) + b)
  bimul a b = ((fromIntegral a) * b)

instance BiNum Float Float where
  type BNTF Float Float = Float
  biadd = (+)
  bimul = (*)

instance BiNum Float Int where 
  type BNTF Float Int = Float
  biadd a b = (a + (fromIntegral b))
  bimul a b = (a * (fromIntegral b))

------------------------------------------------------------------------}}}
-- Expressions                                                          {{{

-- | Data level representation of K3 expression, indexed by equivalent
-- type in Haskell.
class K3 (r :: * -> *) where

  declVar   :: (pt ~ K3Proxied t, t ~ K3Unproxy r pt)
            => Decl s pt -> r t

  -- | Reference the given variable (and promise that it has type 'a')
  --
  -- Note that this is, for example, the only way of producing Targets.
  --
  -- We might also want an eLocalVar :: VarIx -> UnivTyRepr a -> r (Ref a)
  -- to get something like "The K3 Way" of doing local variables?
  unsafeVar :: VarIx -> UnivTyRepr a -> r a

  -- | Add a comment to some part of the AST
  cComment  :: String -> r a -> r a
  -- | Add some annotations to some part of the AST
  cAnn      :: r a -> [AnnE a] -> r a

  cAddress  :: AddrIx -> r AddrIx
  cBool     :: Bool -> r Bool
  cByte     :: Word8 -> r Word8
  cFloat    :: Float -> r Float
  cInt      :: Int -> r Int
  cNothing  :: r (Maybe a)
  cString   :: String -> r String
  cUnit     :: r ()

  eJust     :: r a -> r (Maybe a)
  -- XXX k3ref
  -- eRef      :: r a -> r (Ref r a)

    -- XXX TUPLES
  eTuple2   :: (r a, r b) -> r (a,b)
  eTuple3   :: (r a, r b, r c) -> r (a,b,c)
  eTuple4   :: (r a, r b, r c, r d) -> r (a,b,c,d)
  eTuple5   :: (r a, r b, r c, r d, r e) -> r (a,b,c,d,e)
  -- eTuple    :: K3RTuple r a -> r a

  eHL       :: HRList r a -> r (HList a)

  eEmpty    :: CollTy c -> r (CTE r c e)
  eSing     :: CollTy c -> r e -> r (CTE r c e)
  eCombine  :: r (CTE r c e) -> r (CTE r c e) -> r (CTE r c e)
  eRange    :: r Int -> r Int -> r Int -> r (CTE r c Int)

  eAdd      :: (BiNum a b) => r a -> r b -> r (BNTF a b)
  eMul      :: (BiNum a b) => r a -> r b -> r (BNTF a b)
  eNeg      :: (UnNum a)   => r a -> r a 

    -- XXX Constraints?
  eEq       :: (K3BaseTy a) => r a -> r a -> r Bool
  eLt       :: (K3BaseTy a) => r a -> r a -> r Bool
  eLeq      :: (K3BaseTy a) => r a -> r a -> r Bool
  eNeq      :: (K3BaseTy a) => r a -> r a -> r Bool

  -- | A lambda application in K3.
  --
  -- Unlike traditional lambdas, we require some hints as to how to
  -- destructure the argument (PatTy w) for the lambda; consider (\x -> x)
  -- (1,2) vs (\(x,y) -> eTuple2 (x,y)) (1,2): the former has a HOAS lambda
  -- of type (r (a,b) -> r (a,b)) while the latter has ((r a, r b) -> r
  -- (a,b)).
  eLam      :: (Pat w)
            => PDat UnivTyRepr w -> (PatReprFn r w -> r b) -> r (PatTy r w -> b)

  -- | Apply
  eApp      :: r (a -> b) -> r a -> r b

  -- | Sequence a series of side-effectful statements.  Compare 'sequence_'
  eBlock    :: [r ()] -> r a -> r a

  -- | Apply a side-effecting function for each element of a collection.
  -- Compare 'mapM_'
  eIter     :: r (t -> ()) -> r (CTE r c t) -> r ()

  -- | If-Then-Else
  eITE      :: r Bool -> r a -> r a -> r a

  eMap      :: r (t -> t') -> r (CTE r c t) -> r (CTE r c t')
  eFiltMap  :: r (t -> Bool) -> r (t -> t') -> r (CTE r c t) -> r (CTE r c t')

  eFlatten  :: r (CTE r c (CTE r c' t)) -> r (CTE r c' t)

  -- | Fold over a collection.  Note that collection ordering is not
  -- always well-defined; this should probably be used only with associative
  -- and commutative reducers.
  eFold     :: r ((t', t) -> t') -> r t' -> r (CTE r c t) -> r t'

  -- | Group-By-Aggregate.
  --
  -- Note that the Fold argument is guaranteed to be called
  -- once per partition: any partitions that would be the Zero
  -- are not created by the Partitioner.
  eGBA      :: r (t -> t'')       -- ^ Partitioner
            -> r ((t',t) -> t')   -- ^ Folder
            -> r t'               -- ^ Zero for each partition
            -> r (CTE r c t)      -- ^ Input collection
            -> r (CTE r c (t'',t'))

  -- | Sort a collection into a list.
  eSort     :: r (CTE r c t)      -- ^ Input collection
            -> r ((t,t) -> Bool)  -- ^ Less-or-equal
            -> r (CTE r 'CKList t)

  -- | Peek an element from a collection.
  --
  -- Fails on empty collections.
  --
  -- For lists, this returns the head; for sets and bags
  -- it samples arbitrarily.
  ePeek     :: r (CTE r c e) -> r e

  -- | Slice out from a collection; the slice's type and
  -- the type of elements of the collection must match.
  --
  -- Rather like lambdas, except that the witness is also
  -- a mandatory part of the definition of "slice" :)
  eSlice    :: (Pat w, PatTy r w ~ t, K3BaseTy t)
            => PDat r w      -- ^ Slice specification
            -> r (CTE r c t) -- ^ Input collection
            -> r (CTE r c t)

  eInsert   :: r (CTE r c t) -> r t -> r ()
  eDelete   :: r (CTE r c t) -> r t -> r ()

  -- | Replace an element with another in a collection.
  --
  -- XXX What does this do on 'CTBag'?
  eUpdate   :: r (CTE r c t) -> r t -> r t -> r ()

  -- | Assign to a reference.
  --
  -- Note that dereference is done by a lambda pattern.  See Automation's
  -- 'deref'.
  -- XXX k3ref
  -- eAssign   :: r (Ref r t) -> r t -> r ()

  -- | Send a function and data to another node.
  --
  -- XXX Is there any way to refer to "self" as an addrix?
  eSend     :: r AddrIx -> r (Target r a) -> r a -> r ()

------------------------------------------------------------------------}}}
-- Roles and Streams                                                    {{{

{- XXX UNTESTED!

data Role = Role
data SFormat = CSV | JSON
data Stream t = StreamFile SFormat String

-- | Describe the information flow through a K3 program.  K3 calls these
-- "roles".
--
-- XXX Doesn't do anything with "patterns" at the moment.
class K3RoleDesc (r :: * -> *) where
  type K3RD_M r :: * -> * -> *

  -- | Finalize a role description.  Since we force sources to be made
  -- inside some @Monad@, we'll need a way out which seals the
  -- universe.  This is the ST Monad trick over again.
  mkRole :: (m ~ K3RD_M r)
         => (forall s . (Monad (m s) => m s (r ())))
         -> r Role

  -- | Create a source for this role.
  rSource  :: (K3BaseTy t, m ~ K3RD_M r, Monad (m s))
             => UnivTyRepr t -> Stream t -> m s (r t)

  -- | Bind a source to a trigger
  rBind    :: (K3BaseTy t)
           => r t -> (Decl s t) -> r ()

  -- | Consume
  rConsume :: r t -> r ()

  -- | Many terminal role descriptions group together
  -- to form a terminal role description.
  rBlock :: [r ()] -> r ()
-}

------------------------------------------------------------------------}}}
-- Declarations                                                         {{{

-- | K3 supports a few kinds of delcarations at the top level.
--
-- Those with actual structural content are required to be universal in the
-- underlying representation.  The types exposed here are all 'K3Proxied' so
-- that universally quantified variables do not escape (all references to
-- representation are replaced with 'Proxy').
data DBody dt where
  -- | Collections
  --
  -- XXX No initializers? [r t] ->
  DColl :: (pt ~ K3Proxied t)
        => UnivTyRepr (CTE r c t) -> DBody (CTE Proxy (c :: CKind) pt)

{-
 - XXX K3ref
  -- | Global References
  DRef  :: DBody (Ref Proxy (K3Proxied t))
-}

  -- | Triggers, which execute in a different transaction than the caller
  --
  -- XXX does not support local variables
  DTrig :: (forall r . (K3 r) => r (t -> ())) -> DBody (Target Proxy (K3Proxied t))

  -- | Functions, which execute in the same transaction as the caller
  DFunc :: (forall r . (K3 r) => r (a -> b)) -> DBody (K3Proxied (a -> b))

{- XXX
  -- | Role declaration
  DRole :: (K3RoleDesc ro) => String -> ro Role -> DBody Role
-}

-- | A top-level declaration.
--
-- Contains the name ultimately used in the K3 program and the body of the
-- declaration.
data Decl s t = Decl VarIx (DBody t)

-- XXX This really can't quite be right; in the end a K3 program is a set of
-- role declarations!
data Prog = forall s t . Prog (Decl s t)

newtype DeclIx = DeclIx Int
 deriving (Num,Show)

mkCollDecl :: (Monad m)
           => String
           -> (forall r . Proxy r -> UnivTyRepr (CTE r c t))
           -> MkK3T m s (Decl s (CTE Proxy c (K3Proxied t)))
mkCollDecl n f = do
  (DeclIx uniq) <- incState
  let v = Var $ n ++ "_" ++ show uniq
  return $ Decl v $ DColl (f Proxy)

-- | Define a fixed-point declaration, with an assist in constraining
-- polymorphism.
mkTrigDecl :: (Monad m)
           => String
           -> (forall r . Proxy r -> UnivTyRepr t)
           -> (forall r . (K3 r) => r t -> r (t -> ()))
           -> MkK3T m s (Decl s (Target Proxy (K3Proxied t)))
mkTrigDecl n ty mk     = do
  (DeclIx uniq) <- incState
  let v = Var $ n ++ "_" ++ show uniq
  return $ Decl v (DTrig $ mk (unsafeVar v (ty Proxy)))

newtype MkK3T m s a = MkK3T { unMkK3T :: StateT DeclIx m a }
 deriving (Monad, MonadState DeclIx)

mkK3T :: (Functor m, Monad m)
      => (forall s . MkK3T m s (Decl s a))
      -> m Prog
mkK3T a = Prog `fmap` evalStateT (unMkK3T a) (DeclIx 0)

mkK3 :: (forall s . MkK3T Identity s (Decl s a))
     -> Prog
mkK3 = runIdentity . mkK3T

------------------------------------------------------------------------}}}
-- Utilities for AST work                                               {{{

asR :: r a -> Proxy r -> r a
asR = const

-- | Ensure that the representation type of a collection matches
--
-- This is probably most useful when s is a K3Ty and r is a K3, but this may
-- be more generally applicable.
asCollR :: s (CTE r c t) -> Proxy r -> s (CTE r c t)
asCollR = const

{-
 - XXX k3ref
-- | Ensure that the representation type of a ref matches
asRefR :: s (Ref r t) -> Proxy r -> s (Ref r t)
asRefR = const
-}

------------------------------------------------------------------------}}}
