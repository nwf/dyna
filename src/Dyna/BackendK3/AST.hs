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

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.BackendK3.AST (
    -- * Preliminaries
    VarIx(..), AddrIx(..), Target,

    -- * Numeric Autocasting
    BiNum(..), UnNum(..),

    -- * Collections
    CTE, CKind(..), CollTy(..), K3_Coll_C, asColl,

    -- * References
    Ref, 

    -- * Pattern System
    PKind(..), Pat(..),
    PVar(..), PUnk(..), PJust(..), PRef(..),
    UnPatDa,
    MapPatDa, UnMapPatDa, MapPatTy,
    MapPatConst, K3_Pat_C, K3_Slice_C,

    -- * Annotations
    AnnT(..), AnnE(..), FunDepSpec(..), K3_Xref_C,

    -- * Type System: Base constraints
    K3BaseTy,

    -- * Type System: Representations
    K3Ty(..), UnivTyRepr(..),

    -- * Expressions
    K3(..), 

    -- * Declarations
    Decl(..), DKind(..), mkdecl, mkfdecl, asCollR, asRefR
) where

import           Data.Proxy
import           Data.Word
import           GHC.Prim (Constraint)
-- import           Language.Haskell.TH (varT, mkName)

import           Dyna.XXX.HList
import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

  -- XXX
newtype VarIx  = Var String
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
-- Targets                                                              {{{

-- | The 'r' representation of a target taking argument type 't'.
--
-- This is similar to @t -> ()@ except that it executes in a different
-- transaction and must be named.  The only safe source of Targets is a
-- 'Decl', but being first class, they can be stored in collections for
-- dynamic dispatch.
data family Target (r :: * -> *) t :: *

instance (K3BaseTy a) => K3BaseTy (Target r a)

------------------------------------------------------------------------}}}
-- Collections                                                          {{{

-- | Reflect 'CollTy' at the type level.
data CKind = CBag | CList | CSet

-- | The 'r' representation of a collection of kind 'c' of elements 'e'.
data family CTE (r :: * -> *) (c :: CKind) e

-- | Data-level specification of collection kinds
data CollTy c where
  CTBag  :: CollTy CBag
  CTList :: CollTy CList
  CTSet  :: CollTy CSet

instance (K3BaseTy a) => K3BaseTy (CTE r c a)

-- | A representation-specific constraint for collections, on functions
-- which need to dispatch on a type-tag in the output.
type family K3_Coll_C (r :: * -> *) (c :: CKind) :: Constraint

-- | A convenience function for setting the type of a collection.
--
-- Use as (eEmpty `asColl` CTSet)
asColl :: r (CTE r c t) -> CollTy c -> r (CTE r c t)
asColl = const

------------------------------------------------------------------------}}}
-- References                                                           {{{

-- | The 'r' representation of references of elements of type 'a'
data family Ref (r :: * -> *) a

instance (K3BaseTy a) => K3BaseTy (Ref r a)

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

  -- | Ref patterns (see 'PRef')
  PKRef :: PKind -> PKind

  -- | HList patterns
  PKHL :: [PKind] -> PKind

  -- | Product ("tuple") patterns
  --
  -- 'PatTy' and 'PatReprFn' both produce tuples.
  PKTup  :: [PKind] -> PKind

-- | Provides witnesses that certain types may be used
--   as arguments to K3 lambdas.  Useful when building
--   up type signatures and pattern matches in lambdas.
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
--   The 'r' class parameter (also on 'PVar') is the representation of
--   components of the pattern itself, and is used to constrain
--   representation of variables.  When being used as slices, @r ~ s@; when
--   being used as patterns for lambdas, @r ~ UnivTyRepr@ and 's' is a 'K3'.
--   In this latter case, 'PatDa' is needed for "Dyna.BackendK3.Render"'s
--   function, since every lambda needs an explicit type signature on its
--   variable.
--
class (UnPatDa (PatDa w) ~ w) => Pat (r :: * -> *) (w :: PKind) where
  -- | Any data this witness needs to carry around
  type PatDa w :: *
  -- | The type this witness witnesses (i.e. the things matched against)
  type PatTy (s :: * -> *) w :: *
  -- | The type this witness binds (i.e. after matching is done)
  type PatBTy (s :: * -> *) w :: *
  -- | The type of this pattern.
  type PatReprFn (s :: * -> *) w :: *

-- | Given a chunk of pattern data, recover the PKind.
type family UnPatDa (pd :: *) :: PKind

-- | Given a PatReprFn output and type constructor input, recover the PKind
type family UnPatReprFn (s :: * -> *) (prf :: *) :: PKind

-- | A variable used literally in a pattern
newtype PVar r a = PVar (r a)
type instance UnPatDa (PVar r a) = PKVar r a
instance (K3BaseTy a, r ~ r') => Pat r' (PKVar (r :: * -> *) (a :: *)) where
  type PatDa       (PKVar r a) = PVar r a
  type PatTy     s (PKVar r a) =   a
  type PatBTy    s (PKVar r a) =   a
  type PatReprFn s (PKVar r a) = s a

-- | A pattern wildcard.
--
-- Note that 'PatReprFn s (PUnk a) ~ ()', which should prohibit even
-- accidental use as part of a 'K3' expression or type.
data PUnk (a :: *) = PUnk
type instance UnPatDa (PUnk a)       = PKUnk a
instance (K3BaseTy a) => Pat r (PKUnk (a :: *)) where
  type PatDa       (PKUnk a) = PUnk a
  type PatTy     s (PKUnk a) =   a
  type PatBTy    s (PKUnk a) =   ()
  type PatReprFn s (PKUnk a) =   ()

-- | A /Just/ pattern.  Eliminates a Just constructor, and causes the
-- trigger to abort on a Nothing.
--
-- Note the distinction between PatTy and (PatBTy and PatReprFn) here!
-- This pattern witnesses a type "Maybe a" but binds a variable of type
-- "a".  This will in general be true of any variant (i.e. sum) pattern.
newtype PJust w = PJust { unPJust :: PatDa w }
type instance UnPatDa (PJust w)      = PKJust w
instance (Pat r w) => Pat r (PKJust w) where
  type PatDa       (PKJust w) = PJust w
  type PatTy     s (PKJust w) = Maybe (PatTy s w)
  type PatBTy    s (PKJust w) = PatBTy s w
  type PatReprFn s (PKJust w) = PatReprFn s w

-- | A /Ref/ pattern; dereferences the provided reference.
newtype PRef w = PRef { unPRef :: PatDa w }
type instance UnPatDa (PRef w)    = PKRef w
instance (Pat r w) => Pat r (PKRef w) where
  type PatDa       (PKRef w) = PRef w
  type PatTy     s (PKRef w) = Ref s (PatTy s w)
  type PatBTy    s (PKRef w) = PatBTy s w
  type PatReprFn s (PKRef w) = PatReprFn s w

-- ** Tuples

type family TMapPatDa (x :: [PKind]) :: *
$(mkTyMapFlat 0 ''TMapPatDa ''PatDa)

type family UnMapPatDa (x :: *) :: [PKind]
$(mkTyUnMap Nothing 0 ''UnMapPatDa ''UnPatDa)

type family TMapPatTy (s :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatTy ''PatTy)

type family TMapPatBTy (s :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatBTy ''PatBTy)

type family TMapPatReprFn  (r :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''TMapPatReprFn ''PatReprFn)

$(mkTyUnMap (Just 'PKTup) 0 ''UnPatDa ''UnPatDa)

type family MapPatConst (x :: [PKind]) (r :: * -> *) :: Constraint
type instance MapPatConst '[] r = ()
type instance MapPatConst (x ': xs) r = (Pat r x, MapPatConst xs r)

instance (UnPatDa (TMapPatDa ts) ~ PKTup ts, MapPatConst ts r)
      => Pat     r (PKTup (ts :: [PKind])) where
  type PatDa       (PKTup ts) = TMapPatDa ts
  type PatTy     s (PKTup ts) = TMapPatTy s ts
  type PatBTy    s (PKTup ts) = TMapPatBTy s ts
  type PatReprFn s (PKTup ts) = TMapPatReprFn s ts

-- ** HLists

type instance UnMapPatDa (HList '[]) = '[]
type instance UnMapPatDa (HList (a ': as)) = UnPatDa a ': (UnMapPatDa (HList as))
type instance UnPatDa (HList x) = PKHL (UnMapPatDa (HList x))

type family MapPatDa (x :: [PKind]) :: [*]
type instance MapPatDa ('[]) = '[]
type instance MapPatDa (w ': ws) = PatDa w ': (MapPatDa ws)

type family MapPatTy (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatTy r ('[]) = '[]
type instance MapPatTy r (w ': ws) = PatTy r w ': (MapPatTy r ws)

type family MapPatBTy (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatBTy r ('[]) = '[]
type instance MapPatBTy r (w ': ws) = PatBTy r w ': (MapPatBTy r ws)

type family MapPatReprFn  (r :: * -> *) (x :: [PKind]) :: [*]
type instance MapPatReprFn r '[] = '[]
type instance MapPatReprFn r (w ': ws) = PatReprFn r w ': (MapPatReprFn r ws)

instance (UnPatDa (HList (MapPatDa ts)) ~ PKHL ts, MapPatConst ts r)
      => Pat     r (PKHL (ts :: [PKind])) where
  type PatDa       (PKHL ts) = HList (MapPatDa ts)
  type PatTy     s (PKHL ts) = HList (MapPatTy s ts)
  type PatBTy    s (PKHL ts) = HList (MapPatBTy s ts)
  type PatReprFn s (PKHL ts) = HList (MapPatReprFn s ts)

-- | A representation-specific constraint on handling patterns, on 'eLam'.
type family K3_Pat_C (r :: * -> *) (w :: PKind) :: Constraint

-- | A representation-specific constraint for slices, on 'eSlice'.
type family K3_Slice_C (r :: * -> *) (w :: PKind) :: Constraint

------------------------------------------------------------------------}}}
-- Annotations                                                          {{{

-- | Specification for functional dependencies within a collection.
--
-- XXX This has a phantom type only so that we can use it as an r
-- in RTupled.  We'd rather not (see "Dyna.BackendK3.Render"'s
-- need to use fdscast)
data FunDepSpec a = FDIrr -- ^ /Irr/elevant to a fundep
                  | FDDom -- ^ In the /Dom/ain of a fundep
                  | FDCod -- ^ In the /Cod/omain of a fundep
 deriving (Eq,Show)

-- | A representation-specific constraint for cross-references
type family K3_Xref_C (r :: * -> *) (w :: PKind) :: Constraint

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

  -- | A cross-reference within this collection
  --
  -- XXX this is not actually implemented anywhere and has yet to be
  -- demonstrated as being implementable
  AXref :: (PatTy r w ~ t, w ~ UnPatDa (PatDa w))
        => PatDa w -> (forall p . PatReprFn p w -> p x)
             -- Foreign projection
        -> PatDa w -> (forall p . PatReprFn p w -> p x)
        -> AnnT (CTE r c' t)

  -- | A cross-reference to a declared collection.
  --
  -- XXX this is not actually implemented anywhere and has yet to be
  -- demonstrated as being implementable
  AXrefF :: (Pat p w, Pat p w', PatTy p w ~ t, PatTy p w' ~ t')
         => Decl UnivTyRepr r' (CTE r' c t) -- Foreign collection
         -> PatDa w
         -> (PatReprFn p w  -> p x)         -- Foreign projection
         -> PatDa w'
         -> (PatReprFn p w' -> p x)         -- Local projection
         -> AnnT (CTE r c' t')

  -- | An escape hatch! (XXX)
  ATMisc :: String -> AnnT a


-- | Annotations on 'K3' expressions
data AnnE a where

  -- | Decorate an expression as atomic.
  AAtomic :: AnnE a

  -- |
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
  tRef     :: r a -> r (Ref r' a)
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
  tRef   (UTR a)         = UTR $ tRef a

  -- XXX TUPLES
  -- tTuple   us            = UTR $ tTuple  $ tupleopRS unUTR us
  tTuple2  us            = UTR $ tTuple2 $ tupleopRS unUTR us
  tTuple3  us            = UTR $ tTuple3 $ tupleopRS unUTR us
  tTuple4  us            = UTR $ tTuple4 $ tupleopRS unUTR us
  tTuple5  us            = UTR $ tTuple5 $ tupleopRS unUTR us

  tHL      us            = UTR $ tHL     $ hrlmap unUTR us

------------------------------------------------------------------------}}}
-- Numeric Autocasting                                                  {{{

  -- XXX should we make these be constraints in the K3 class so that
  -- different representations can make different choices?

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


  -- XXX More

------------------------------------------------------------------------}}}
-- Expressions                                                          {{{

-- | Data level representation of K3 expression, indexed by equivalent
-- type in Haskell.
class K3 (r :: * -> *) where

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

  -- | Reference the given variable (and promise that it has type 'a')
  --
  -- Note that this is, for example, the only way of producing Targets.
  --
  -- XXX replace with something more like declvar so that, in theory, a
  -- sufficiently smart "r" might know what to do about it?
  --
  -- We might also want an eLocalVar :: VarIx -> UnivTyRepr a -> r (Ref a)
  -- to get something like "The K3 Way" of doing local variables?
  unsafeVar :: VarIx -> UnivTyRepr a -> r a

  declVar   :: Decl UnivTyRepr r a -> r a

  eJust     :: r a -> r (Maybe a)
  eRef      :: r a -> r (Ref r a)

    -- XXX TUPLES
  eTuple2   :: (r a, r b) -> r (a,b)
  eTuple3   :: (r a, r b, r c) -> r (a,b,c)
  eTuple4   :: (r a, r b, r c, r d) -> r (a,b,c,d)
  eTuple5   :: (r a, r b, r c, r d, r e) -> r (a,b,c,d,e)
  -- eTuple    :: K3RTuple r a -> r a

  eHL       :: HRList r a -> r (HList a)

  eEmpty    :: (K3_Coll_C r c) => r (CTE r c e)
  eSing     :: (K3_Coll_C r c) => r e -> r (CTE r c e)
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
  eLam      :: (K3_Pat_C r w, Pat UnivTyRepr w, K3BaseTy (PatTy r w))
            => PatDa w -> (PatReprFn r w -> r b) -> r (PatTy r w -> b)

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
            -> r (CTE r 'CList t)

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
  eSlice    :: (K3_Slice_C r w, Pat r w, PatTy r w ~ t)
            => PatDa w       -- ^ Slice specification
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
  eAssign   :: r (Ref r t) -> r t -> r ()

  -- | Send a function and data to another node.
  --
  -- XXX Is there any way to refer to "self" as an addrix?
  eSend     :: r AddrIx -> r (Target r a) -> r a -> r ()

------------------------------------------------------------------------}}}
-- Declarations                                                         {{{

-- | K3 supports a few kinds of delcarations at the top level:
data DKind r dt where
  -- | Collections
  --
  -- XXX No initializers? [t] ->
  DKColl :: DKind r (CTE r (c :: CKind) t)

  -- | Global References
  DKRef  :: DKind r (Ref r t)

  -- | Functions, which execute in the same transaction as the caller
  DKFunc :: r (a -> b) -> DKind r (a -> b)

  -- | Triggers, which execute in a different transaction than the caller
  DKTrig :: r (t -> ()) -> DKind r (Target r t)

-- | A top-level declaration.
--
-- XXX does not enumerate local variables
data Decl tr r t = Decl VarIx (tr t) (DKind r t)

-- | A utility for setting the type of sub-components of a declaration, by
-- constraining polymorphism.  Use the 'asCollR' and 'asRefR' combinators
-- to avail yourself of the Proxy passed in.
mkdecl :: (Proxy r -> Decl tr r t) -> Decl tr r t
mkdecl f = f Proxy

-- | Define a fixed-point declaration.  Like mkdecl, it continues to assist
-- in constraining polymorphism, but also yields a representation of the
-- declaration being made.
--
-- Note that this relies on laziness in Haskell it pulls out the name and
-- type fields of the Decl being built to construct a K3 AST variable to
-- refer to the current definition.
mkfdecl :: (K3 r, K3Ty trx)
        => (Proxy r -> r t -> (forall tr . (K3Ty tr) => Decl tr r t))
        -> Decl trx r t
mkfdecl f = let self = (\(Decl n tr _) -> unsafeVar n tr) (f Proxy self)
            in f Proxy self

-- | Ensure that the representation type of a collection matches
--
-- This is probably most useful when s is a K3Ty and r is a K3, but this may
-- be more generally applicable.
asCollR :: s (CTE r c t) -> Proxy r -> s (CTE r c t)
asCollR = const

-- | Ensure that the representation type of a ref matches
asRefR :: r' (Ref r t) -> Proxy r -> r' (Ref r t)
asRefR = const

------------------------------------------------------------------------}}}
