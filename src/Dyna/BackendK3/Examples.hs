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
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Example cases: misc
------------------------------------------------------------------------{{{


  -- | Perform a simple join of two collections using a predicate and apply
  -- some function to rows that match.
  --
  -- This is intended to be sufficiently simple for K3 to chew on and
  -- do something useful with in its optimizer backend.
macro_simple_join2 :: (K3 r, K3AutoTy a, K3BaseTy a, K3AST_Pat_C r (PKVar a),
                             K3AutoTy b, K3BaseTy b, K3AST_Pat_C r (PKVar b))
                   => r (a -> b -> Bool) -> r (a -> b -> ())
                   -> r (CTE c1 a) -> r (CTE c2 b) -> r ()
macro_simple_join2 p f c1 c2 =
    flip eIter c1 $ eLam (PVar autoty) $ \a ->
    flip eIter c2 $ eLam (PVar autoty) $ \b ->
      eITE (eApp (eApp p a) b) (eApp (eApp f a) b) (cUnit)


testdecf = Decl (Var "f")
                (tColl CTBag (tTuple2 (tInt,tInt)))
                Nothing

testmfn = Decl (Var "negAddOne")
               (tFun tInt tInt)
               $Just (eLam (PVar tInt) (\a -> eNeg $ eAdd a $ cInt 1))

booli = Decl (Var "booli")
             (tFun tBool tInt)
             $ Just (eLam (PVar tBool) (\b -> eITE b (cInt 1) (cInt 0)))

testcfn = Decl (Var "cfn")
               (tFun tInt $ tColl CTSet tInt)
               $Just (eLam (PVar tInt) (\x -> eSing x))

testpairfn = Decl (Var "ibfst")
                  (tFun (tTuple2 (tInt,tBool)) tInt)
                  $Just (eLam (PTup (PVar tInt, PVar tBool)) (\(a,b) -> a))

lamslice = eLam (PVar autoty) $ \a ->
             eSlice (STup (SVar a, SVar (cInt 4)))
                    (eSing (eTuple2 (cInt 3, cInt 4)) `asColl` CTSet)

    -- XXX Man we need better tuple handling.
project = eLam (PTup (PVar autoty, PVar autoty))
               $ \(x,c) -> eMap (eLam (PTup (PVar autoty
                                            ,PVar autoty
                                            ,PVar autoty))
                                      $ \(_,y,z) -> eTuple2 (y,z))
                                (eSlice (STup (SVar x, SUnk, SUnk)) c)

proj' = eLam (PTup (PVar tInt, PVar tInt, PVar tInt))
           $ \(a,b,c) -> b

    -- Sum-Singleton case from M3ToK3 test
    -- It is safe to leave off the top-level signature
sumsing (ix :: r Int) (iy :: r Int) c1 c2 = eAdd (v c1) (v c2)
 where
    -- It is safe to eliminate this type signature
  si = STup (SVar ix,SVar iy,SUnk)

    -- XXX unfortunately, we have to be explicit about the forall c1 here;
    -- eliminating this type signature results in unified collection types
    -- for c1 and c2 above.
  v :: (K3AST_Pat_C r ('PKVar (CTE c1 (Int, Int, Int))),
       K3AST_Coll_C r c1, K3AutoColl c1)
    => r (CTE c1 (Int, Int, Int)) -> r Int
  v c = eApp (eLam (PVar autoty)
                   (\cv -> macro_emptyPeek
                             cv (cInt 0)
                             (\nec -> eApp (eLam (PTup (PVar autoty
                                                       ,PVar autoty
                                                       ,PVar autoty))
                                           $ \(_,_,proj) -> proj)
                                         nec)))
            (eSlice si c)

    -- A very very complicated way of writing "3"
testSumsing = sumsing (cInt 4) (cInt 5)
                      (eSing (eTuple3 (cInt 4, cInt 5, cInt 1)) `asColl` CTSet)
                      (eSing (eTuple3 (cInt 4, cInt 5, cInt 2)) `asColl` CTBag)

testjoin2 c1 c2 =
    macro_simple_join2 pred c1 c2
 where p = PTup (PVar tInt, PVar tInt, PVar tInt)
       pred = (eLam p (\(k1a,k2a,_) ->
               eLam p (\(k1b,k2b,_) ->
                (eEq k1a k1b) `eAdd` (eEq k2a k2b))))


testlocal = macro_localVar autoty
                           (eEmpty `asColl` CTBag)
                           (\x -> eInsert x $ eTuple2 (cInt 3, cInt 4))


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
