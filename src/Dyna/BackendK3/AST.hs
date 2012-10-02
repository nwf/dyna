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

module Dyna.BackendK3.AST where

import           Data.Word
import           GHC.Prim (Constraint)
import           Language.Haskell.TH (varT, mkName)

import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

newtype VarIx  = Var String
newtype AddrIx = Addr (String,Int)

data Ann = Ann [String]

------------------------------------------------------------------------}}}
-- Collections                                                          {{{

data CKind = CBag | CList | CSet

data CTE (c :: CKind) e

data CollTy c where
  CTBag  :: CollTy CBag
  CTList :: CollTy CList
  CTSet  :: CollTy CSet

------------------------------------------------------------------------}}}
-- Effectables (XXX TODO)                                               {{{

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

data Ref a = Ref

------------------------------------------------------------------------}}}
-- Type System                                                          {{{

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

  -- tPair   :: r a -> r b -> r (a,b)
  tMaybe  :: r a -> r (Maybe a)
  tRef    :: r a -> r (Ref a)
  tColl   :: CollTy c -> r a -> r (CTE c a)
  tFun    :: r a -> r b -> r (a -> b)

  -- tTuple  :: (RTupled rt, RTR rt ~ r) => rt -> r (RTE rt)

    -- XXX TUPLES
  tTuple2 :: (r a, r b) -> r (a,b)
  tTuple3 :: (r a, r b, r c) -> r (a,b,c)
  tTuple4 :: (r a, r b, r c, r d) -> r (a,b,c,d)

  -- | Existential typeclass wrapper for K3Ty
newtype UnivTyRepr (a :: *) = UTR { unUTR :: forall r . (K3Ty r) => r a }

instance K3Ty UnivTyRepr where
  tAnn   s (UTR t)               = UTR $ tAnn s t
  tBool                          = UTR tBool
  tByte                          = UTR tByte
  tFloat                         = UTR tFloat
  tInt                           = UTR tInt
  tString                        = UTR tString
  tUnit                          = UTR tUnit
  tUnk                           = UTR tUnk

  tColl  c (UTR a)       = UTR $ tColl c a
  tFun   (UTR a) (UTR b) = UTR $ tFun a b
  tMaybe (UTR a)         = UTR $ tMaybe a
  tRef   (UTR a)         = UTR $ tRef a

  -- XXX TUPLES
  tTuple2  us              = UTR $ tTuple2 $ tupleopRS unUTR us
  tTuple3  us              = UTR $ tTuple3 $ tupleopRS unUTR us
  tTuple4  us              = UTR $ tTuple4 $ tupleopRS unUTR us


  -- | A constraint for "base" types in K3.  These are the things that can
  -- be passed to lambdas.  Essentially everything other than arrows.
class    K3BaseTy a
instance K3BaseTy Bool
instance K3BaseTy Word8
instance K3BaseTy Float
instance K3BaseTy Int
instance K3BaseTy String
instance K3BaseTy ()
instance (K3BaseTy a) => K3BaseTy (CTE c a)
instance (K3BaseTy a) => K3BaseTy (Maybe a)
instance (K3BaseTy a) => K3BaseTy (Ref a)
instance (K3BaseTy a, K3BaseTy b) => K3BaseTy (a,b)
instance (K3BaseTy a, K3BaseTy b, K3BaseTy c) => K3BaseTy (a,b,c)

------------------------------------------------------------------------}}}
-- Pattern System                                                       {{{

  -- | Kinds of patterns permitted in K3
data PKind where
  PKVar  :: k -> PKind
  PKJust :: PKind -> PKind

    -- XXX TUPLES
  PKTuple2 :: (PKind, PKind) -> PKind
  PKTuple3 :: (PKind, PKind, PKind) -> PKind
  PKTuple4 :: (PKind, PKind, PKind, PKind) -> PKind
  -- PKTup  :: [PKind] -> PKind

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
class Pat (w :: PKind) where
    -- | Any data this witness needs to carry around
  data PatDa w :: *
    -- | The type this witness witnesses (i.e. the things matched against)
  type PatTy w :: *
    -- | The type this witness binds (i.e. after matching is done)
  type PatBTy w :: *
    -- | The type of this pattern.
  type PatReprFn w (r :: * -> *) :: *

instance (K3BaseTy a) => Pat (PKVar (a :: *)) where
  data PatDa     (PKVar a)   = PVar { unPVar :: UnivTyRepr a }
  type PatTy     (PKVar a)   =   a
  type PatBTy    (PKVar a)   =   a
  type PatReprFn (PKVar a) r = r a

instance (Pat w) => Pat (PKJust w) where
  -- | Just patterns (fail on Nothing)
  --
  -- Note the distinction between PatTy and (PatBTy and PatReprFn) here!
  -- This pattern witnesses a type "Maybe a" but binds a variable of type
  -- "a".  This will in general be true of any variant (i.e. sum) pattern.
  data PatDa (PKJust w)       = PJust (PatDa w)
  type PatTy (PKJust w)       = Maybe (PatTy w)
  type PatBTy (PKJust w)      = PatBTy w
  type PatReprFn (PKJust w) r = PatReprFn w r

{-
instance (Pat wa, Pat wb) => Pat (PKPair '(wa,wb)) where
  -- | Pair patterns
  --
  -- Product patterns, on the other hand, have PatTy and PatReprFn both
  -- producing tuples.
  data PatDa (PKPair '(wa,wb))       = PPair (PatDa wa) (PatDa wb)
  type PatTy (PKPair '(wa,wb))       = (PatTy wa, PatTy wb)
  type PatBTy (PKPair '(wa,wb))      = (PatBTy wa, PatBTy wb)
  type PatReprFn (PKPair '(wa,wb)) r = (PatReprFn wa r, PatReprFn wb r)
-}

  -- XXX TUPLES
$( mapM (mkRecClass (''Pat,[]) (\n -> mkName $ "PKTuple" ++ show n)
                         [(''PatDa,\n -> mkName $ "PTuple" ++ show n )]
                         [''PatTy, ''PatBTy] [''PatReprFn])
        [2..4]
 )

{-
  -- Tragically patterns based on tuples are still represented
  -- in Haskell as right-branching, unit-ending.
instance Pat     (PKTup '[]) where
  data PatDa     (PKTup '[])   = PTupN
  type PatTy     (PKTup '[])   = ()
  type PatBTy    (PKTup '[])   = ()
  type PatReprFn (PKTup '[]) r = r ()

instance Pat (PKTup as) => Pat (PKTup (a ': as)) where
  data PatDa  (PKTup (a ': as))      = PTupC (PatDa  a) (PatDa  (PKTup as))
  type PatTy  (PKTup (a ': as))      =       (PatTy  a,  PatTy  (PKTup as))
  type PatBTy (PKTup (a ': as))      =       (PatBTy a,  PatBTy (PKTup as))
  type PatReprFn (PKTup (a ': as)) r = (PatReprFn a r, PatReprFn (PKTup as) r)
-}

------------------------------------------------------------------------}}}
-- Slice System                                                         {{{

  -- | Kinds of slices permitted in K3
data SKind where
  SKVar  :: r -> k -> SKind
  SKUnk  :: k -> SKind
  SKJust :: SKind -> SKind

    -- XXX TUPLES
  SKTuple2 :: (SKind, SKind) -> SKind
  SKTuple3 :: (SKind, SKind, SKind) -> SKind
  SKTuple4 :: (SKind, SKind, SKind, SKind) -> SKind

  -- | Witness of slice well-formedness
class Slice r (w :: SKind) where
  data SliceDa w :: *
  type SliceTy w :: *

instance (K3BaseTy a, r0 ~ r) => Slice r0 (SKVar (r :: * -> *) (a :: *)) where
  data SliceDa (SKVar r a) = SVar (r a)
  type SliceTy (SKVar r a) = a

instance (K3BaseTy a) => Slice r (SKUnk (a :: *)) where
  data SliceDa (SKUnk a) = SUnk
  type SliceTy (SKUnk a) = a

instance (Slice r s) => Slice r (SKJust s) where
  data SliceDa (SKJust s) = SJust (SliceDa s)
  type SliceTy (SKJust s) = Maybe (SliceTy s)

{-
instance (Slice r sa, Slice r sb) => Slice r (SKTuple2 '(sa,sb)) where
  data SliceDa (SKTuple2 '(sa,sb)) = STuple2 (SliceDa sa) (SliceDa sb)
  type SliceTy (SKTuple2 '(sa,sb)) = (SliceTy sa, SliceTy sb)
-}

  -- XXX TUPLES
$( mapM (mkRecClass (''Slice, [varT $ mkName "r"])
                    (\n -> mkName $ "SKTuple" ++ show n)
                    [(''SliceDa,\n -> mkName $ "STuple" ++ show n)]
                    [''SliceTy] [])
        [2..4] ) 


{-
instance Slice r (SKTup '[]) where
  data SliceDa (SKTup '[]) = STupN
  type SliceTy (SKTup '[]) = ()

instance Slice r (SKTup as) => Slice r (SKTup (a ': as)) where
  data SliceDa (SKTup (a ': as)) = STupC (SliceDa a) (SliceDa (SKTup as))
  type SliceTy (SKTup (a ': as)) =       (SliceTy a,  SliceTy (SKTup as))

-}

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
-- Values and Expressions                                               {{{

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
    --
    -- The K3 AST has this but uses it for wildcards in slices, which
    -- we handle with SKUnk/SUnk.
  -- cUnk      :: r a 

    -- XXX cAddress  :: AddrIx -> r AddrIx
  cBool     :: Bool -> r Bool
  cByte     :: Word8 -> r Word8
  cFloat    :: Float -> r Float
  cInt      :: Int -> r Int
  cNothing  :: r (Maybe a)
  cString   :: String -> r String
  cUnit     :: r ()

  eVar      :: VarIx -> UnivTyRepr a -> r a

  eJust     :: r a -> r (Maybe t)

    -- XXX TUPLES
  eTuple2   :: (r a, r b) -> r (a,b)
  eTuple3   :: (r a, r b, r c) -> r (a,b,c)
  eTuple4   :: (r a, r b, r c) -> r (a,b,c)
  -- eTuple    :: K3RTuple r a -> r a

  eEmpty    :: (K3AST_Coll_C r c) => r (CTE c e)
  eSing     :: (K3AST_Coll_C r c) => r e -> r (CTE c e)
  eCombine  :: r (CTE c e) -> r (CTE c e) -> r (CTE c e)
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
  eLam      :: (K3AST_Pat_C r w, Pat w, K3BaseTy (PatTy w))
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

    -- | Group-By-Aggregate.
    --
    -- Note that the Fold argument is guaranteed to be called
    -- once per partition: any partitions that would be the Zero
    -- are not created by the Partitioner.
  eGBA      :: r (t -> t'')       -- ^ Partitioner
            -> r ((t',t) -> t')   -- ^ Folder
            -> r t'               -- ^ Zero for each partition
            -> r (CTE c t)        -- ^ Input collection
            -> r (CTE c (t'',t'))

  eSort     :: r (CTE c t)        -- ^ Input collection
            -> r ((t,t) -> Bool)  -- ^ Less-or-equal
            -> r (CTE 'CList t)

    -- | Peek an element from a collection.
    --
    -- Fails on empty collections.
    --
    -- For lists, this returns the head; for sets and bags
    -- it samples arbitrarily.
  ePeek     :: r (CTE c e) -> r e

    -- | Slice out from a collection; the slice's type and
    -- the type of elements of the collection must match.
    --
    -- Rather like lambdas, except that the witness is also
    -- a mandatory part of the definition of "slice" :)
  eSlice    :: (K3AST_Slice_C r w, Slice r w, SliceTy w ~ t)
            => SliceDa w -> r (CTE c t) -> r (CTE c t)

  eInsert   :: r (CTE c t) -> r t -> r ()
  eDelete   :: r (CTE c t) -> r t -> r ()
  eUpdate   :: r (CTE c t) -> r t -> r t -> r ()

  eAssign   :: r (Ref t) -> r t -> r ()
  eDeref    :: r (Ref t) -> r t

  -- XXX eSend

------------------------------------------------------------------------}}}
-- Miscellany                                                           {{{

  -- XXX does not enumerate local variables
data Decl tr r t = Decl VarIx (tr t) (Maybe (r t))

  -- | A convenience function for setting the type of a collection.
  --
  -- Use as (eEmpty `asColl` CTSet)
asColl :: r (CTE c t) -> CollTy c -> r (CTE c t)
asColl = const
------------------------------------------------------------------------}}}
