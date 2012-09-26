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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Dyna.BackendK3.Examples where

import           Dyna.BackendK3.AST
import           Dyna.BackendK3.Automation
import           Dyna.BackendK3.Render

------------------------------------------------------------------------}}}
-- Example cases: macros
------------------------------------------------------------------------{{{

macro_caseMaybe :: (K3 r, K3BaseTy a, K3AST_Pat_C r (PKJust (PKVar a)))
                => UnivTyRepr a
                -> r (Maybe a)
                -> r b
                -> (r a -> r b)
                -> r b
macro_caseMaybe w m n b = eITE (eEq m cNothing)
                               n
                               (eApp (eLam (PJust (PVar w)) b) m)

test_macroCM = Decl (Var "nocase")
                    (tInt)
                    $Just $ macro_caseMaybe tInt (eVar (Var "test") autoty) (cInt 0) (id)

macro_simple_join2 :: (K3 r, K3AutoTy a, K3BaseTy a, K3AST_Pat_C r (PKVar a),
                             K3AutoTy b, K3BaseTy b, K3AST_Pat_C r (PKVar b))
                   => r (CTE c1 a) -> r (CTE c2 b) -> r (a -> b -> Bool) -> r ()
macro_simple_join2 c1 c2 p =
    flip eIter c1 $ eLam (PVar autoty) $ \a -> flip eIter c2
                  $ eLam (PVar autoty) $ \b -> eITE (eApp (eApp p a) b) (cUnit) (cUnit)

macro_emptyPeek :: (K3AST_Coll_C r c, K3AST_Pat_C r (PKVar a),
                    K3 r, K3BaseTy a, K3AutoTy a)
                => r (CTE c a) -> r b -> (r a -> r b) -> r b
macro_emptyPeek c e l = eITE (eEq c eEmpty)
                             e
                             (eApp (eLam (PVar autoty) l) $ ePeek c)

------------------------------------------------------------------------}}}
-- Example cases: misc
------------------------------------------------------------------------{{{


testdecf = Decl (Var "f")
                (tColl CTBag (tPair tInt tInt))
                Nothing

testmfn = Decl (Var "negAddOne")
               (tFun tInt tInt)
               $Just (eLam (PVar $ UTR tInt) (\a -> eNeg $ eAdd a $ cInt 1))

booli = Decl (Var "booli")
             (tFun tBool tInt)
             $ Just (eLam (PVar (UTR tBool)) (\b -> eITE b (cInt 1) (cInt 0)))

testcfn = Decl (Var "cfn")
               (tFun tInt $ tColl CTSet tInt)
               $Just (eLam (PVar tInt) (\x -> eSing x))


testpairfn = Decl (Var "ibfst")
                  (tFun (tPair tInt tBool) tInt)
                  $Just (eLam (PPair (PVar tInt) (PVar tBool)) (\(a,b) -> a))

lamslice = eLam (PVar autoty) $ \a ->
             eSlice (SPair (SVar a) (SVar (cInt 4)))
                    (eSing (ePair (cInt 3) (cInt 4)) `asColl` CTSet)

    -- XXX Man we need better tuple handling.
project = eLam (PPair (PVar autoty) (PVar autoty))
               $ \(x,c) -> eMap (eLam (PPair (PVar autoty)
                                      (PPair (PVar autoty)
                                             (PVar autoty)))
                                      $ \(_,(y,z)) -> ePair y z)
                                (eSlice (SPair (SVar x) (SPair SUnk SUnk)) c)

    -- Sum-Singleton case from M3ToK3 test
    -- It is safe to leave off the top-level signature
sumsing :: (K3 r, K3AutoColl c, K3AutoColl c',
           K3AST_Coll_C r c,
           K3AST_Coll_C r c',
           K3AST_Pat_C r (PKVar (Int, (Int, Int))),
           K3AST_Pat_C r (PKVar (CTE c (Int, (Int, Int)))),
           K3AST_Pat_C r (PKVar (CTE c' (Int, (Int, Int)))),
           K3AST_Pat_C r (PKPair (PKVar Int) (PKPair (PKVar Int) (PKVar Int))),
           K3AST_Slice_C r (SKPair (SKVar r Int) (SKPair (SKVar r Int) (SKUnk Int)))
           )
        => r Int -> r Int
        -> r (CTE c (Int, (Int,Int))) -> r (CTE c' (Int, (Int,Int)))
        -> r Int
sumsing (ix :: r Int) iy c1 c2 = eAdd (v c1) (v c2)
 where
    -- It is safe to eliminate this type signature
  si :: SliceDa (SKPair (SKVar r Int) (SKPair (SKVar r Int) (SKUnk Int)))
  si = SPair (SVar ix) (SPair (SVar iy) SUnk)

    -- XXX unfortunately, we have to be explicit about the forall c1 here;
    -- eliminating this type signature results in unified collection types
    -- for c1 and c2 above.
  v :: (K3AST_Pat_C r ('PKVar (CTE c1 (Int, (Int, Int)))),
       K3AST_Coll_C r c1, K3AutoColl c1)
    => r (CTE c1 (Int, (Int, Int))) -> r Int
  v c = eApp (eLam (PVar autoty)
                   (\cv -> macro_emptyPeek
                             cv (cInt 0)
                             (\nec -> eApp (eLam (PPair (PVar autoty)
                                                 (PPair (PVar autoty)
                                                        (PVar autoty)))
                                           (\(_,(_,proj)) -> proj))
                                         nec)))
            (eSlice si c)

------------------------------------------------------------------------}}}
-- Example cases: misc badness
------------------------------------------------------------------------{{{

-- We can write this with undefined, but it will induce a
-- constraint K3BaseTy (Bool -> b).  Note that if b ever
-- became monomorphic, the search would fail; further,
-- trying to fill in the undefined will make it monomorphic. :)
--
-- That is, we are prevented from ever actually writing this thing out
-- to the K3 compiler.
testHOF = eLam (PVar undefined) $ \x -> eApp x (cBool True)

------------------------------------------------------------------------}}}
-- fin
---------------------------------------------------------------------------
