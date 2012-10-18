---------------------------------------------------------------------------
-- | An AST for K3.
--
-- To as large of an extent as possible, we wish to capture the static 
-- semantics of K3 in the Haskell type system.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.BackendK3.AST where

import           Data.Word
import           GHC.Prim (Constraint)
import           Language.Haskell.TH (varT, mkName)

import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
{- * Preliminaries -} --                                                {{{

  -- XXX
newtype VarIx  = Var String
  -- XXX (Hostname,Port)
newtype AddrIx = Addr (String,Int)
 deriving (Eq,Show)

    -- XXX This has a phantom type only so that we can use it as an r
    -- in RTupled.  We'd rather not (see .Render's need to use fdscast)
data FunDepSpec a = FDIrr | FDDom | FDCod
 deriving (Eq,Show)

  -- XXX should really do something smarter
data Ann a where

  -- | Decorate an expression as atomic.
  AAtomic :: Ann a

  -- | A functional dependency among elements of a collection.
  AFunDep :: (RTupled fs, RTE fs ~ a, RTR fs ~ FunDepSpec)
          => fs -> Ann (CTE r t a)

  -- XXX Declare an additional index
  -- AIndex :: (RTupled fs, RTE fs ~ a, RTR fs ~ FunDepSpec)
  --        => fs -> Ann (CTE r t a)

  -- | An "Exactly-One-Of" annotation, used to convey variants (i.e. sums)
  --   to K3.
  AOneOf  :: (RTupled mv, RTR mv ~ Maybe) => Ann mv

  -- | An escape hatch! (XXX)
  AMisc :: String -> Ann a

------------------------------------------------------------------------}}}
{- * Collections -} --                                                  {{{

data CKind = CBag | CList | CSet

data family CTE (r :: * -> *) (c :: CKind) e

data CollTy c where
  CTBag  :: CollTy CBag
  CTList :: CollTy CList
  CTSet  :: CollTy CSet

------------------------------------------------------------------------}}}
{- * Effectables (XXX TODO) -} --                                       {{{

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

data family Ref (r :: * -> *) a

------------------------------------------------------------------------}}}
{- * Type System -} --                                                  {{{

-- | A constraint for "base" types in K3.  These are the things that can
-- be passed to lambdas.  Essentially everything other than arrows.
class    K3BaseTy a
instance K3BaseTy Bool
instance K3BaseTy Word8
instance K3BaseTy Float
instance K3BaseTy Int
instance K3BaseTy String
instance K3BaseTy ()
instance (K3BaseTy a) => K3BaseTy (CTE r c a)
instance (K3BaseTy a) => K3BaseTy (Maybe a)
instance (K3BaseTy a) => K3BaseTy (Ref r a)
$(mkTupleRecInstances ''K3BaseTy [])

-- | Data level representation of K3 types, indexed by equivalent type in
-- Haskell.
class K3Ty (r :: * -> *) where
  -- | Attach an annotation to a type
  tAnn    :: r a -> [Ann a] -> r a

  tAddress :: r AddrIx
  tBool    :: r Bool
  tByte    :: r Word8
  tFloat   :: r Float
  tInt     :: r Int
  tString  :: r String
  tUnit    :: r ()

  -- tPair   :: r a -> r b -> r (a,b)
  tMaybe  :: r a -> r (Maybe a)
  tRef    :: r a -> r (Ref r' a)
  tColl   :: (K3BaseTy a) => CollTy c -> r a -> r (CTE r' c a)
  tFun    :: r a -> r b -> r (a -> b)

  -- XXX TUPLES
  -- tTuple  :: (RTupled rt, RTR rt ~ r, RTE rt ~ t) => rt -> r t
  tTuple2 :: (r a, r b) -> r (a,b)
  tTuple3 :: (r a, r b, r c) -> r (a,b,c)
  tTuple4 :: (r a, r b, r c, r d) -> r (a,b,c,d)
  tTuple5 :: (r a, r b, r c, r d, r e) -> r (a,b,c,d,e)

-- | Universal typeclass wrapper for K3Ty
newtype UnivTyRepr (a :: *) = UTR { unUTR :: forall r . (K3Ty r) => r a }

instance K3Ty UnivTyRepr where
  tAnn   (UTR t) s               = UTR $ tAnn t s

  tAddress                       = UTR tAddress
  tBool                          = UTR tBool
  tByte                          = UTR tByte
  tFloat                         = UTR tFloat
  tInt                           = UTR tInt
  tString                        = UTR tString
  tUnit                          = UTR tUnit

  tColl  c (UTR a)       = UTR $ tColl c a
  tFun   (UTR a) (UTR b) = UTR $ tFun a b
  tMaybe (UTR a)         = UTR $ tMaybe a
  tRef   (UTR a)         = UTR $ tRef a

  -- XXX TUPLES
  -- tTuple   us              = UTR $ tTuple  $ tupleopRS unUTR us
  tTuple2  us              = UTR $ tTuple2 $ tupleopRS unUTR us
  tTuple3  us              = UTR $ tTuple3 $ tupleopRS unUTR us
  tTuple4  us              = UTR $ tTuple4 $ tupleopRS unUTR us
  tTuple5  us              = UTR $ tTuple5 $ tupleopRS unUTR us


------------------------------------------------------------------------}}}
{- * Pattern System -} --                                               {{{

-- | Kinds of patterns permitted in K3
data PKind where
  -- | Variables in patterns
  PKVar  :: k -> PKind

  -- | Wildcards in patterns
  PKUnk  :: k -> PKind

  -- | Just patterns (fail on Nothing)
  --
  -- Note the distinction between PatTy and (PatBTy and PatReprFn) here!
  -- This pattern witnesses a type "Maybe a" but binds a variable of type
  -- "a".  This will in general be true of any variant (i.e. sum) pattern.
  PKJust :: PKind -> PKind

  -- | Product ("tuple") patterns
  --
  -- Product patterns, on the other hand, have PatTy and PatReprFn both
  -- producing tuples.
  PKTup  :: [PKind] -> PKind

-- | Provides witnesses that certain types may be used
--   as arguments to K3 lambdas.  Useful when building
--   up type signatures and pattern matches in lambdas.
--
--   Note that this is a closed class using the promoted
--   data PKind.
--
--   PatDa is needed for Render's function, since every
--   lambda needs an explicit type signature on its variable.
--
--   Essentially, these things determine where "r"s end up
--   in the lambda given to eLam.  Compare and contrast:
--     eLam (PVar $ tMaybe tInt)            :: (r (Maybe Int)  -> _) -> _
--     eLam (PJust $ PVar tInt)             :: (r Int          -> _) -> _
--
--     eLam (PVar $ tPair tInt tInt)        :: (r (Int, Int)   -> _) -> _
--     eLam (PPair (PVar tInt) (PVar tInt)) :: ((r Int, r Int) -> _) -> _
--
class (UnPatDa (PatDa w) ~ w) => Pat (w :: PKind) where
  -- | Any data this witness needs to carry around
  type PatDa w :: *
  -- | The type this witness witnesses (i.e. the things matched against)
  type PatTy w :: *
  -- | The type this witness binds (i.e. after matching is done)
  type PatBTy w :: *
  -- | The type of this pattern.
  type PatReprFn (r :: * -> *) w :: *

type family UnPatDa (pd :: *) :: PKind

data PVar a = PVar (UnivTyRepr a)
type instance UnPatDa (PVar a) = PKVar a
instance (K3BaseTy a) => Pat (PKVar (a :: *)) where
  type PatDa     (PKVar a)   = PVar a
  type PatTy     (PKVar a)   =   a
  type PatBTy    (PKVar a)   =   a
  type PatReprFn r (PKVar a) = r a

data PUnk a = PUnk
type instance UnPatDa (PUnk a)       = PKUnk a
instance (K3BaseTy a) => Pat (PKUnk (a :: *)) where
  type PatDa     (PKUnk a)   = PUnk a
  type PatTy     (PKUnk a)   =   a
  type PatBTy    (PKUnk a)   =   ()
  type PatReprFn r (PKUnk a) = r ()

data PJust a = PJust { unPJust :: a }
type instance UnPatDa (PJust a)      = PKJust (UnPatDa a)
instance (Pat w) => Pat (PKJust w) where
  type PatDa (PKJust w)       = PJust (PatDa w)
  type PatTy (PKJust w)       = Maybe (PatTy w)
  type PatBTy (PKJust w)      = PatBTy w
  type PatReprFn r (PKJust w) = PatReprFn r w

type family MapPatDa (x :: [PKind]) :: *
$(mkTyMapFlat 0 ''MapPatDa ''PatDa)

type family UnMapPatDa (x :: *) :: [PKind]
$(mkTyUnMap Nothing 0 ''UnMapPatDa ''UnPatDa)

type family MapPatTy (x :: [PKind]) :: *
$(mkTyMapFlat 0 ''MapPatTy ''PatTy)

type family MapPatBTy (x :: [PKind]) :: *
$(mkTyMapFlat 0 ''MapPatBTy ''PatBTy)

type family MapPatReprFn   (r :: * -> *) (x :: [PKind]) :: *
$(mkTyMapFlat 1 ''MapPatReprFn ''PatReprFn)

$(mkTyUnMap (Just 'PKTup) 0 ''UnPatDa ''UnPatDa)

instance (UnPatDa (MapPatDa ts) ~ 'PKTup ts)
      => Pat       (PKTup (ts :: [PKind])) where
  type PatDa       (PKTup ts)   = MapPatDa ts
  type PatTy       (PKTup ts)   = MapPatTy ts
  type PatBTy      (PKTup ts)   = MapPatBTy ts
  type PatReprFn r (PKTup ts)   = MapPatReprFn r ts

------------------------------------------------------------------------}}}
{- * Slice System -} --                                                 {{{

-- | Kinds of slices permitted in K3
data SKind where
  SKVar  :: r -> k -> SKind
  SKUnk  :: k -> SKind
  SKJust :: SKind -> SKind

  SKTup  :: [SKind] -> SKind

-- | Witness of slice well-formedness
class (UnSliceDa (SliceDa w) ~ w) => Slice r (w :: SKind) where
  type SliceDa w :: *
  type SliceTy w :: *

type family UnSliceDa (pd :: *) :: SKind

data SVar r a = SVar (r a)
type instance UnSliceDa (SVar r a) = SKVar r a
instance (K3BaseTy a, r0 ~ r) => Slice r0 (SKVar (r :: * -> *) (a :: *)) where
  type SliceDa (SKVar r a) = SVar r a
  type SliceTy (SKVar r a) = a

data SUnk a = SUnk
type instance UnSliceDa (SUnk a)       = SKUnk a
instance (K3BaseTy a) => Slice r (SKUnk (a :: *)) where
  type SliceDa (SKUnk a) = SUnk a
  type SliceTy (SKUnk a) = a

data SJust a = SJust { unSJust :: a }
type instance UnSliceDa (SJust a)      = SKJust (UnSliceDa a)
instance (Slice r s) => Slice r (SKJust s) where
  type SliceDa (SKJust s) = SJust (SliceDa s)
  type SliceTy (SKJust s) = Maybe (SliceTy s)

type family SliceConst (x :: SKind) (r :: * -> *) :: Constraint
type instance SliceConst x r = Slice r x

type family MapSliceConst (x :: [SKind]) (r :: * -> *) :: Constraint
type instance MapSliceConst '[] r = ()
type instance MapSliceConst (x ': xs) r = (SliceConst x r, MapSliceConst xs r)

type family MapSliceDa (x :: [SKind]) :: *
$(mkTyMapFlat 0 ''MapSliceDa ''SliceDa)

type family UnMapSliceDa (x :: *) :: [SKind]
$(mkTyUnMap Nothing 0 ''UnMapSliceDa ''UnSliceDa)

type family MapSliceTy (x :: [SKind]) :: *
$(mkTyMapFlat 0 ''MapSliceTy ''SliceTy)

$(mkTyUnMap (Just 'SKTup) 0 ''UnSliceDa ''UnSliceDa)

instance (UnSliceDa (MapSliceDa ts) ~ 'SKTup ts, MapSliceConst ts r)
      => Slice r (SKTup (ts :: [SKind])) where
  type SliceDa   (SKTup ts)   = MapSliceDa ts
  type SliceTy   (SKTup ts)   = MapSliceTy ts

------------------------------------------------------------------------}}}
{- * Numeric Autocasting -} --                                          {{{

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
{- * Values and Expressions -} --                                       {{{

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
  cAnn      :: r a -> [Ann a] -> r a

    -- XXX An escape hatch
    --
    -- The K3 AST has this but uses it for wildcards in slices, which
    -- we handle with SKUnk/SUnk.
  -- cUnk      :: r a 

  cAddress  :: AddrIx -> r AddrIx
  cBool     :: Bool -> r Bool
  cByte     :: Word8 -> r Word8
  cFloat    :: Float -> r Float
  cInt      :: Int -> r Int
  cNothing  :: r (Maybe a)
  cString   :: String -> r String
  cUnit     :: r ()

  eVar      :: VarIx -> UnivTyRepr a -> r a

  eJust     :: r a -> r (Maybe a)

    -- XXX TUPLES
  eTuple2   :: (r a, r b) -> r (a,b)
  eTuple3   :: (r a, r b, r c) -> r (a,b,c)
  eTuple4   :: (r a, r b, r c,r d) -> r (a,b,c,d)
  -- eTuple    :: K3RTuple r a -> r a

  eEmpty    :: (K3AST_Coll_C r c) => r (CTE r c e)
  eSing     :: (K3AST_Coll_C r c) => r e -> r (CTE r c e)
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
  eLam      :: (K3AST_Pat_C r w, Pat w, K3BaseTy (PatTy w))
            => PatDa w -> (PatReprFn r w -> r b) -> r (PatTy w -> b)
  eApp      :: r (a -> b) -> r a -> r b

  eBlock    :: [r ()] -> r a -> r a

  eIter     :: r (t -> ()) -> r (CTE r c t) -> r ()

  eITE      :: r Bool -> r a -> r a -> r a

  eMap      :: r (t -> t') -> r (CTE r c t) -> r (CTE r c t')
  eFiltMap  :: r (t -> Bool) -> r (t -> t') -> r (CTE r c t) -> r (CTE r c t')

  eFlatten  :: r (CTE r c (CTE r c' t)) -> r (CTE r c' t)

  -- | Called Aggregate in K3's AST
  eFold     :: r ((t', t) -> t') -> r t' -> r (CTE r c t) -> r t'

  -- | Group-By-Aggregate.
  --
  -- Note that the Fold argument is guaranteed to be called
  -- once per partition: any partitions that would be the Zero
  -- are not created by the Partitioner.
  eGBA      :: r (t -> t'')       -- ^ Partitioner
            -> r ((t',t) -> t')   -- ^ Folder
            -> r t'               -- ^ Zero for each partition
            -> r (CTE r c t)        -- ^ Input collection
            -> r (CTE r c (t'',t'))

  eSort     :: r (CTE r c t)        -- ^ Input collection
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
  eSlice    :: (K3AST_Slice_C r w, Slice r w, SliceTy w ~ t)
            => SliceDa w     -- ^ Slice specification
            -> r (CTE r c t) -- ^ Input collection
            -> r (CTE r c t)

  eInsert   :: r (CTE r c t) -> r t -> r ()
  eDelete   :: r (CTE r c t) -> r t -> r ()
  eUpdate   :: r (CTE r c t) -> r t -> r t -> r ()

  eAssign   :: r (Ref r t) -> r t -> r ()
  eDeref    :: r (Ref r t) -> r t

  eSend     :: r AddrIx -> r (a -> ()) -> r a -> r ()

------------------------------------------------------------------------}}}
{- * Miscellany -} --                                                   {{{

  -- XXX does not enumerate local variables
data Decl tr r t = Decl VarIx (tr t) (Maybe (r t))

-- | A convenience function for setting the type of a collection.
--
-- Use as (eEmpty `asColl` CTSet)
asColl :: r (CTE r c t) -> CollTy c -> r (CTE r c t)
asColl = const

------------------------------------------------------------------------}}}
