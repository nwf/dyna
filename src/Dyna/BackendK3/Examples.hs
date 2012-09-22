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


module Dyna.BackendK3.Examples where

import           Dyna.BackendK3.AST
import           Dyna.BackendK3.Render

------------------------------------------------------------------------}}}
-- Example cases
------------------------------------------------------------------------{{{

macro_caseMaybe :: (K3 r, K3AST_Pat_C r (PKJust (PKVar a)))
                => ExTyRepr a
                -> r (Maybe a)
                -> r b
                -> (r a -> r b)
                -> r b
macro_caseMaybe w m n b = eITE (eEq m cNothing)
                                n
                                (eApp (eLam (PJust (PVar w)) b) m)

test_macroCM = Decl (Var "nocase")
                    (tInt)
                    $Just $ macro_caseMaybe tInt (eVar (Var "test")) (cInt 0) (id)

testdecf = Decl (Var "f")
                (tColl CTBag (tPair tInt tInt))
                Nothing

testmfn = Decl (Var "negAddOne")
               (tFun tInt tInt)
               $Just (eLam (PVar $ ETR tInt) (\a -> eNeg $ eAdd a $ cInt 1))

booli = Decl (Var "booli")
             (tFun tBool tInt)
             $ Just (eLam (PVar (ETR tBool)) (\b -> eITE b (cInt 1) (cInt 0)))

testcfn = Decl (Var "cfn")
               (tFun tInt $ tColl CTSet tInt)
               $Just (eLam (PVar tInt) (\x -> eSing x))


testpairfn = Decl (Var "ibfst")
                  (tFun (tPair tInt tBool) tInt)
                  $Just (eLam (PPair (PVar tInt) (PVar tBool)) (\(a,b) -> a))

exslice =  eSlice (SPair (SVar (Var "x") tInt)
                         (SVar (Var "y") tInt))
                  (eSing (ePair (cInt 3) (cInt 4)) `asColl` CTSet)

------------------------------------------------------------------------}}}
-- fin
---------------------------------------------------------------------------
