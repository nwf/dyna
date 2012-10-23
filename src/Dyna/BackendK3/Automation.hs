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

module Dyna.BackendK3.Automation (
    -- * Automated derivation of data from types, where possible
  K3AutoColl, autocoll, K3AutoTy, autoty,

    -- * Pattern handling
  autopv,

    -- * K3 macro library
  localVar, caseMaybe, emptyPeek, deref, combMany
) where

import           Data.Word
import           Dyna.BackendK3.AST
import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Automate collection type                                             {{{

-- | Demote a collection type annotation (of kind CKind) to the
-- appropriate chunk of data for case analysis.
--
-- Note that this only works once the type has become monomorphic;
-- otherwise it imposes a constraint on the haskell tyvar.
-- (This should be a total function)
class K3AutoColl (c :: CKind) where autocoll :: CollTy c
instance K3AutoColl CBag where autocoll = CTBag
instance K3AutoColl CList where autocoll = CTList
instance K3AutoColl CSet where autocoll = CTSet

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
instance (K3AutoTy a) => K3AutoTy (Ref r a) where autoty = tRef autoty
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
{- * Automate pattern -} -- XXX                                         {{{

{-
-- | Automatically derive a pattern, for use with eLam.
-- Note that this is only useful for the (common) case of not using Just
-- patterns.

class (Pat UnivTyRepr w) => K3AutoPat (w :: PKind) where
  autopat :: PatDa w

instance (K3BaseTy a, K3AutoTy a) => K3AutoPat (PKVar UnivTyRepr a) where
  autopat = PVar autoty

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

autopv :: (K3AutoTy a) => PatDa (PKVar UnivTyRepr a)
autopv = PVar autoty

------------------------------------------------------------------------}}}
-- K3 Macro Library (XXX WIP)                                           {{{

-- | Let as lambda
localVar :: (K3 r, K3AutoTy a, K3BaseTy a, K3_Pat_C r (PKVar UnivTyRepr a))
         => (r a)
         -> (r a -> r b)
         -> r b
localVar a b = eApp (eLam autopv b) a

{-
-- | Let as lambda with explicit type variable
localVar' :: (K3 r, K3BaseTy a, K3_Pat_C r (PKVar UnivTyRepr a))
          => UnivTyRepr a
          -> (r a)
          -> (r a -> r b)
          -> r b
localVar' w a b = eApp (eLam (PVar w) b) a
-}

-- | Case analyze a Maybe
caseMaybe :: (K3 r, K3BaseTy a,
              K3_Pat_C r (PKJust (PKVar UnivTyRepr a)))
          => UnivTyRepr a
          -> r (Maybe a)
          -> r b
          -> (r a -> r b)
          -> r b
caseMaybe w m n b = eITE (eEq m cNothing)
                               n
                               (eApp (eLam (PJust (PVar w)) b) m)

-- | Case analyze a collection as either empty or a peeked element
emptyPeek :: (K3_Coll_C r c,
              K3_Pat_C r (PKVar UnivTyRepr a),
              K3 r, K3BaseTy a, K3AutoTy a)
          => r (CTE r c a) -> r b -> (r a -> r b) -> r b
emptyPeek c e l = eITE (eEq c eEmpty)
                             e
                             (eApp (eLam (PVar autoty) l) $ ePeek c)

-- | Dereference a value by using a Ref pattern on a lambda.
deref :: (K3_Pat_C r (PKRef (PKVar UnivTyRepr a)),
          K3AutoTy a, K3BaseTy a, K3 r)
      => r (Ref r a) -> r a
deref = eApp (eLam (PRef $ PVar autoty) id)

-- | Combine many things
combMany :: (K3_Coll_C r c, K3 r) => [r a] -> r (CTE r c a)
combMany = foldr (\e c -> eCombine c (eSing e)) eEmpty

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
  type K3AST_Pat_C VarsInK3 p = ()
  type K3AST_Slice_C VarsInK3 s = ()

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
