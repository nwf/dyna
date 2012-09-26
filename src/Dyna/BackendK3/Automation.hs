---------------------------------------------------------------------------
-- Header material
------------------------------------------------------------------------{{{
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
{-# LANGUAGE UndecidableInstances #-}

module Dyna.BackendK3.Automation where

import           Data.Word
import           Text.PrettyPrint.Free

import           Dyna.BackendK3.AST
import           Dyna.BackendK3.Render

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
instance (K3AutoColl c, K3AutoTy a) => K3AutoTy (CTE c a) where
  autoty = tColl autocoll autoty
instance (K3AutoTy a) => K3AutoTy (Maybe a) where autoty = tMaybe autoty
instance (K3AutoTy a) => K3AutoTy (Ref a) where autoty = tRef autoty
instance (K3AutoTy a, K3AutoTy b) => K3AutoTy (a,b) where
  autoty = tPair autoty autoty
instance (K3AutoTy a, K3AutoTy b) => K3AutoTy (a -> b) where
  autoty = tFun autoty autoty

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
