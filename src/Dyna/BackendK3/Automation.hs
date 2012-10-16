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

    -- * K3 macro library
  macro_localVar, macro_caseMaybe, macro_emptyPeek
) where

import           Data.Word
import           Dyna.BackendK3.AST

------------------------------------------------------------------------}}}
-- Automate collection type                                             {{{

-- | Demote a collection type annotation (of kind CKind) to the
-- appropriate chunk of data for case analysis.
--
-- Note that this only works once the type has become monomorphic;
-- otherwise it imposes a constraint on the haskell tyvar.
-- (This is a total function)
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
-- (This is a total function)
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
instance (K3AutoTy a, K3AutoTy b) => K3AutoTy (a -> b) where
  autoty = tFun autoty autoty

  -- XXX TUPLES
instance (K3AutoTy a, K3AutoTy b) => K3AutoTy (a,b)
 where autoty = tTuple2 (autoty, autoty)

instance (K3AutoTy a, K3AutoTy b, K3AutoTy c) => K3AutoTy (a,b,c)
 where autoty = tTuple3 (autoty, autoty, autoty)

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
-- K3 Macro Library (XXX WIP)                                           {{{

-- | Let as lambda
macro_localVar :: (K3 r, K3BaseTy a, K3AST_Pat_C r (PKVar a))
                => UnivTyRepr a
                -> (r a)
                -> (r a -> r b)
                -> r b
macro_localVar w a b = eApp (eLam (PVar w) b) a

-- | Case analyze a Maybe
macro_caseMaybe :: (K3 r, K3BaseTy a, K3AST_Pat_C r (PKJust (PKVar a)))
                => UnivTyRepr a
                -> r (Maybe a)
                -> r b
                -> (r a -> r b)
                -> r b
macro_caseMaybe w m n b = eITE (eEq m cNothing)
                               n
                               (eApp (eLam (PJust (PVar w)) b) m)

-- | Case analyze a collection as either empty or a peeked element
macro_emptyPeek :: (K3AST_Coll_C r c, K3AST_Pat_C r (PKVar a),
                    K3 r, K3BaseTy a, K3AutoTy a)
                => r (CTE r c a) -> r b -> (r a -> r b) -> r b
macro_emptyPeek c e l = eITE (eEq c eEmpty)
                             e
                             (eApp (eLam (PVar autoty) l) $ ePeek c)

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
