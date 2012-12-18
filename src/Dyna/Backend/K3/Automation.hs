---------------------------------------------------------------------------
--  | Various automation assists for working with K3 ASTs

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.Backend.K3.Automation (
    -- * Automated derivation of data from types, where possible
  K3AutoColl, autocoll, K3AutoTy, autoty,

    -- * Pattern handling
  autopv,

    -- * K3 macro library
  localVar, caseMaybe, emptyPeek, {- XXX k3ref deref, -} combMany, switchCase
) where

import           Data.Word
import           Dyna.Backend.K3.AST
import           Dyna.XXX.HList
-- import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Automate collection type                                             {{{

-- | Demote a collection type annotation (of kind CKind) to the
-- appropriate chunk of data for case analysis.
--
-- Note that this only works once the type has become monomorphic;
-- otherwise it imposes a constraint on the haskell tyvar.
-- (This should be a total function)
class K3AutoColl (c :: CKind) where autocoll :: CollTy c
instance K3AutoColl CKBag where autocoll = CBag
instance K3AutoColl CKList where autocoll = CList
instance K3AutoColl CKSet where autocoll = CSet

------------------------------------------------------------------------}}}
-- Automate type                                                        {{{

-- | Attempt to automatically derive a universal type representation.
--
-- Note that this only works once the type has become monomorphic;
-- otherwise it imposes a constraint on the haskell tyvar.
-- (This should be a total function)
class    K3AutoTy a      where autoty :: UnivTyRepr a
instance K3AutoTy Bool   where autoty = tBool
instance K3AutoTy Word8  where autoty = tByte
instance K3AutoTy Float  where autoty = tFloat
instance K3AutoTy Int    where autoty = tInt
instance K3AutoTy String where autoty = tString
instance K3AutoTy ()     where autoty = tUnit
instance (K3AutoColl c, K3AutoTy a, K3BaseTy a) => K3AutoTy (CTE r c a) where
  autoty = tColl autocoll autoty
instance (K3AutoTy a) => K3AutoTy (Maybe a) where autoty = tMaybe autoty
-- XXX k3ref
-- instance (K3AutoTy a) => K3AutoTy (Ref r a) where autoty = tRef autoty
instance (K3AutoTy a) => K3AutoTy (Target r a) where autoty = tTarget autoty
instance (K3AutoTy a, K3BaseTy a, K3AutoTy b, K3BaseTy b)
      => K3AutoTy (a -> b) where
  autoty = tFun autoty autoty

  -- HList derivation
class K3AutoTyHL (a :: [*]) where autotyhl :: HRList UnivTyRepr a
instance K3AutoTyHL '[] where autotyhl = HRN
instance (K3AutoTy a, K3AutoTyHL as)
      => K3AutoTyHL (a ': as) where autotyhl = autoty :++ autotyhl

instance K3AutoTyHL a
      => K3AutoTy (HList a) where autoty = tHL autotyhl

  -- XXX TUPLES
instance (K3AutoTy a, K3AutoTy b) => K3AutoTy (a,b)
 where autoty = tTuple2 (autoty, autoty)
instance (K3AutoTy a, K3AutoTy b, K3AutoTy c) => K3AutoTy (a,b,c)
 where autoty = tTuple3 (autoty, autoty, autoty)
instance (K3AutoTy a, K3AutoTy b, K3AutoTy c, K3AutoTy d) => K3AutoTy (a,b,c,d)
 where autoty = tTuple4 (autoty, autoty, autoty, autoty)

{-
class (Pat (PKTup ws), PatTy (PKTup ws) ~ a) => K3AutoTyTup ws a
      | ws -> a, a -> ws
 where autotytup :: K3RTuple UnivTyRepr a

instance K3AutoTyTup '[] () where autotytup = K3RTNil

instance (K3AutoTyTup was as, K3AutoTy a, wa ~ PKVar a, PatTy wa ~ a)
         => K3AutoTyTup (wa ': was) (a,as)
 where autotytup = K3RTCons autoty autotytup

instance (K3AutoTyTup (wa ': w) (a,b), K3AutoTyTup w b)
         => K3AutoTy (a,b)
 where autoty = tTuple autotytup
-}

------------------------------------------------------------------------}}}
-- Automate pattern (XXX)                                               {{{

-- | Automatically derive a pattern, for use with eLam.
-- Note that this is only useful for the (common) case of not using
-- elimination patterns.

{-

type family   UnPatReprFn (s :: * -> *) (pr :: *) :: PKind
type instance UnPatReprFn s (s a) = PKVar UnivTyRepr a
type instance UnPatReprFn s (HList '[]) = PKHL '[]

class (Pat UnivTyRepr w) => K3AutoPat (w :: PKind) where
  autopat :: PatDa w

instance (K3BaseTy a, K3AutoTy a) => K3AutoPat (PKVar UnivTyRepr a) where
  autopat = PVar autoty

instance K3AutoPat (PKHL '[]) where
  autopat = HN

instance (K3AutoPat (PKHL ws),
          K3AutoPat w,
          MapPatConst ws UnivTyRepr)
      => K3AutoPat (PKHL (w ': ws)) where
  autopat = autopat :+ autopat

class UFAP (w :: [PKind]) where unfoldautopat :: HList (MapPatDa w)
instance UFAP '[] where unfoldautopat = HN

instance (UnPatDa (PatDa a) ~ a, K3AutoPat a,
          UFAP as, MapPatDa (a ': as) ~ (PatDa a ': (MapPatDa as))
         )
      => UFAP (a ': as) where unfoldautopat =    (autopat :: PatDa a)
                                              :+ (unfoldautopat :: HList (MapPatDa as))

instance (UnPatDa (PatDa a) ~ a, K3AutoPat a,
          UnPatDa (PatDa b) ~ b, K3AutoPat b)
      => UFAP '[a,b] where unfoldautopat =    (autopat :: PatDa a)
                                           :+ (autopat :: PatDa b)
                                           :+ HN
instance (K3AutoTy a, UFAP ts)
      => K3AutoPat (PKTup ts) where
  autopat = hlTuple $ unfoldautopat

-}

autopv :: (K3BaseTy a, K3AutoTy a) => PDat UnivTyRepr (PKVar UnivTyRepr a)
autopv = PVar autoty

------------------------------------------------------------------------}}}
-- K3 Macro Library (XXX WIP)                                           {{{

-- | Let as lambda
localVar :: (K3 r, K3AutoTy a, K3BaseTy a)
         => (r a)
         -> (r a -> r b)
         -> r b
localVar a b = eApp (eLam autopv b) a

{-
-- | Let as lambda with explicit type variable
localVar' :: (K3 r, K3BaseTy a)
          => UnivTyRepr a
          -> (r a)
          -> (r a -> r b)
          -> r b
localVar' w a b = eApp (eLam (PVar w) b) a
-}

-- | Case analyze a Maybe
caseMaybe :: (K3 r, K3BaseTy a)
          => UnivTyRepr a
          -> r (Maybe a)
          -> r b
          -> (r a -> r b)
          -> r b
caseMaybe w m n b = eITE (eEq m cNothing)
                               n
                               (eApp (eLam (PJust (PVar w)) b) m)

-- | Case analyze a collection as either empty or a peeked element
emptyPeek :: (K3 r, K3BaseTy a, K3AutoTy a, K3AutoColl c)
          => r (CTE r c a) -> r b -> (r a -> r b) -> r b
emptyPeek c e l = eITE (eEq c $ eEmpty autocoll)
                             e
                             (eApp (eLam (PVar autoty) l) $ ePeek c)

{- XXX k3ref
-- | Dereference a value by using a Ref pattern on a lambda.
deref :: (K3AutoTy a, K3BaseTy a, K3 r)
      => r (Ref r a) -> r a
deref = eApp (eLam (PRef $ PVar autoty) id)
-}

-- | Combine many things
combMany :: (K3AutoColl c, K3 r) => [r a] -> r (CTE r c a)
combMany = foldr (\e c -> eCombine c (eSing autocoll e)) $ eEmpty autocoll

switchCase :: (K3 r, K3AutoTy s, K3BaseTy s)
           => [(r s, r a)] -> r a -> r (s -> a)
switchCase arms def = eLam autopv
  (\s -> foldl (\c (s',a) -> eITE (eEq s s') a c) def arms)

------------------------------------------------------------------------}}}
-- Collect variables in a term (XXX TODO)                               {{{

{-
data ExVarTy = forall t . EVT VarIx (UnivTyRepr t)

showEVT :: ExVarTy -> Doc e
showEVT evt = case evt of EVT (Var vn) utr ->     text vn
                                              <+> colon
                                              <+> unAsK3Ty (unUTR utr)

newtype VarsInK3 a = VIK [ExVarTy]

sVIK :: VarsInK3 t -> Doc e
sVIK (VIK vs) = list $ map showEVT vs

instance K3 VarsInK3 where
  type K3AST_Coll_C VarsInK3 c = ()

  cComment _ v = v
  cAnn _ a = a

  cBool  _ = VIK []
  cByte  _ = VIK []
  cFloat _ = VIK []

  eVar vi r = let x = VIK [EVT vi r] in x

  eIter (VIK f) (VIK c) = VIK $ f ++ c

  -- XXX etc
-}

------------------------------------------------------------------------}}}
