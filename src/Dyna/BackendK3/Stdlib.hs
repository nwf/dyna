---------------------------------------------------------------------------
-- | A standard library for the K3 backend.  A collection of canned
-- routines.
--
-- Unlike Dyna.BackendK3.Automation, this is intended specifically for the
-- purpose of implementing Dyna-on-K3.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Dyna.BackendK3.Stdlib (
    -- * Generic term storage
    -- ** Haskell type level
    TermIx, ArgIx, FAIx, TXRef, IndTy, IndTyRow, IndTyRowSet,

    -- ** K3 type level
    ttermix, targix, tfaix, txref, indty,

    -- ** K3 data representation
    fatab, faixCtr, heap,

    -- ** K3 representation mathematics
    foldMath, foldMathSum, foldMathProd,

    -- ** K3 heap utilities
    allocTermixHelper, allocTermix, allocFaix, findFaix, initHeapRow,

    -- ** Metaprogramming term representation utilities
    -- *** Data Injections
    indterm, indint, indflt, indstr,
    -- *** Slice Injections
    sindterm, sindint, sindflt, sindstr,
    -- *** Projectors
    proterm, proint, proflt, prostr,

    -- ** Metaprogramming heap utilities
    hproindr, hpropara, hproeval

) where

import           Control.Monad.State
import           Dyna.BackendK3.AST
import           Dyna.BackendK3.Automation
import           Dyna.XXX.HList
import           Dyna.XXX.MonadUtils
import           Dyna.XXX.THTuple

------------------------------------------------------------------------}}}
-- Generic term storage, Haskell type level                             {{{

type TermIx = Int
type ArgIx = Int
type FAIx = Int
type TXRef = (FAIx, TermIx)
type IndTy = HList '[Maybe TXRef, Maybe Int, Maybe Float, Maybe String]
type IndTyRow = (Int, Int, IndTy)
type IndTyRowSet r = CTE r CSet IndTyRow

------------------------------------------------------------------------}}}
-- Generic term storage, K3 type level                                  {{{
--
-- The type-annotations here serve to make the Haskell toolchain's output
-- prettier; being type aliases, though, there is no semantic difference
-- between these and the inferred types.

-- | TERM IndeX
ttermix :: (K3Ty r) => r TermIx
ttermix = tInt

-- | ARGument IndeX
targix :: (K3Ty r) => r ArgIx
targix  = tInt

-- | Functor and Arity IndeX
tfaix :: (K3Ty r) => r FAIx
tfaix   = tInt

-- | Term crossreference
txref = tTuple2 $ (tfaix, ttermix)

-- | A node in a recursive term structure
--
-- This needs more base-cases.
indty :: K3Ty r => r IndTy
indty  = tHL
           (   tMaybe txref       -- Term xref
           :++ tMaybe tInt        -- Base int
           :++ tMaybe tFloat      -- Base float
           :++ tMaybe tString     -- Base string
           :++ HRN
           )
         `tAnn` [AOneOfHL]

------------------------------------------------------------------------}}}
-- Generic term storage, K3 data representation                         {{{

-- | For each pair of functor and arity, what is the name we've given?
--
-- Used when we don't know static assignments
fatab = flip (Decl $ Var "fatab") DKColl $
        tColl CTSet (tTuple3 (tfaix, tString, tInt))
            `tAnn` [ AFunDep (FDDom,FDCod,FDCod)
                   , AFunDep (FDCod,FDDom,FDDom)
                   ]

-- | A counter for dynamic allocation of faixes
faixCtr = mkdecl $ \r -> Decl (Var "faixCtr") (tRef tfaix `asRefR` r) DKRef

-- | The place we store all our terms, partitioned globally by functor-arity
-- values.  For each faix, we store
--
--   * a counter for the next possibly-free termix,
--
--   * a table of arguments
--
--   * a table of parasitic functors (which should be thought of as Dyna's
--   version of newtypes: top-level f/1 which are eliminated from
--   evaluations at compile time.)
--
--   * a table of evaluations (including parasitic evaluations)
heap = mkdecl $ \r -> flip (Decl $ Var "heap") DKColl $
        (tColl CTSet $
         tHL (   tfaix
             :++ tRef ttermix `asRefR` r
           
             -- Indirection
             :++ tRef ((tColl CTSet $ tTuple3 (ttermix, targix, indty))
                 `tAnn` [AFunDep (FDDom,FDDom,FDCod)]
                 `asCollR` r) `asRefR` r

             -- Parasitism (used when we don't statically know the parasitic
             -- storage "offset")
             :++ tRef ((tColl CTSet $ tTuple2 (tString, tInt))
                 `tAnn` [AFunDep (FDDom,FDCod)]
                 `asCollR` r) `asRefR` r

             -- Evaluation (including parasitism)
             :++ tRef ((tColl CTSet $ tTuple3 (ttermix, tInt, indty))
                 `tAnn` [AFunDep (FDDom,FDDom,FDCod)]
                 `asCollR` r) `asRefR` r

             :++ HRN
             )) 
              `tAnn` [AFunDepHL (FDDom :++ FDCod :++ FDCod :++ FDCod :++ FDCod :++ HRN)
                     , AXrefF fatab
                              (autopv, PUnk, PUnk)
                              (\(x,_,_) -> x)
                              (PVar autoty:+PUnk:+PUnk:+PUnk:+PUnk:+HN)
                              (\(x:+_) -> x)
                     ]
              `asCollR` r

------------------------------------------------------------------------}}}
-- Generic term storage, term representation mathematics                {{{

foldMath (s,fn :: forall a b . (BiNum a b) => r a -> r b -> r (BNTF a b))
  = Decl (Var $ "foldMath" ++ s) (tFun (tTuple2 (indty, indty)) indty) $ DKFunc $
    eLam (PVar indty, PVar indty) $ \(a,b) ->
        localVar (pronum a) $ \aif ->
            localVar (pronum b) $ \bif ->
                eITE (aif `eEq` eTuple2 (cNothing, cNothing)) (indstr $ cString "Error") $
                    eITE (bif `eEq` eTuple2 (cNothing, cNothing)) (indstr $ cString "Error") $
                        eApp (eLam (PVar $ tMaybe tInt, PVar $ tMaybe tFloat) $ \(mai,maf) ->
                                caseMaybe tInt mai (eApp (eLam (PJust $ PVar tFloat)
                                                            $ \a -> commonB a indflt bif
                                                         ) maf)
                                    $ \a -> commonB a indint bif
                             ) aif
 where
  commonB :: (K3 r, BiNum a Float, BNTF a Float ~ Float, BiNum a Int)
          => r a
          -> (r (BNTF a Int) -> r IndTy)
          -> r (Maybe Int, Maybe Float)
          -> r IndTy
  commonB a f bif =
    eApp (eLam (PVar $ tMaybe tInt, PVar $ tMaybe tFloat) $ \(mbi,mbf) ->
            caseMaybe tInt mbi
                (eApp (eLam (PJust $ PVar tFloat) $ \b -> indflt $ a `fn` b) mbf)
                (\b -> f $ a `fn` b)
         ) bif


foldMathSum  = foldMath ("sum" , eAdd)
foldMathProd = foldMath ("prod", eMul)

------------------------------------------------------------------------}}}
-- Generic term storage, metaprogramming term representation            {{{

indterm :: K3 r => r TXRef -> r IndTy
indterm x = eHL (eJust x  :++ cNothing :++ cNothing :++ cNothing :++ HRN)

indint  :: K3 r => r Int -> r IndTy
indint  x = eHL (cNothing :++ eJust x  :++ cNothing :++ cNothing :++ HRN)

indflt  :: K3 r => r Float -> r IndTy
indflt  x = eHL (cNothing :++ cNothing :++ eJust x  :++ cNothing :++ HRN)

indstr  :: K3 r => r String -> r IndTy
indstr  x = eHL (cNothing :++ cNothing :++ cNothing :++ eJust x  :++ HRN)

sindterm :: (K3 r, PatDa w ~ PVar r TXRef)
         => r TXRef
         -> HList [PJust w, PVar r (Maybe Int), PVar r (Maybe Float), PVar r (Maybe String)]
sindterm x = (PJust (PVar x) :+ PVar cNothing  :+ PVar cNothing  :+ PVar cNothing  :+ HN)

sindint  x = (PVar cNothing  :+ PJust (PVar x) :+ PVar cNothing  :+ PVar cNothing  :+ HN)
sindflt  x = (PVar cNothing  :+ PVar cNothing  :+ PJust (PVar x) :+ PVar cNothing  :+ HN)
sindstr  x = (PVar cNothing  :+ PVar cNothing  :+ PVar cNothing  :+ PJust (PVar x) :+ HN)

proterm (x :: r IndTy) =
  flip eApp x $ eLam (PJust (PVar $ tTuple2 (tfaix,ttermix)) :+ PUnk :+ PUnk :+ PUnk :+ HN) 
                     (\(i:+_:+_:+_:+HN) -> i)
proint  (x :: r IndTy) =
  flip eApp x $ eLam (PUnk :+ PJust (PVar tInt) :+ PUnk :+ PUnk :+ HN)  
                     (\(_:+i:+_:+_:+HN) -> i)
proflt  (x :: r IndTy) =
  flip eApp x $ eLam (PUnk :+ PUnk :+ PJust (PVar tFloat) :+ PUnk :+ HN)
                     (\(_:+_:+i:+_:+HN) -> i)
prostr  (x :: r IndTy) =
  flip eApp x $ eLam (PUnk :+ PUnk :+ PUnk :+ PJust (PVar tString) :+ HN)
                     (\(_:+_:+_:+i:+HN) -> i)

pronum (x :: r IndTy) =
  flip eApp x $ eLam (PUnk :+ PVar (tMaybe tInt) :+ PVar (tMaybe tFloat) :+ PUnk :+ HN)
           (\(_:+i:+j:+_:+HN) -> eTuple2 (i,j))

------------------------------------------------------------------------}}}
-- Generic term storage, heap utility functions                         {{{

-- | Set up a new faix on the heap
initHeapRow = Decl (Var "initHeapRow") (tFun tfaix tUnit) $ DKFunc $ 
  eLam autopv (\faix -> eInsert (declVar heap)
                                $ eHL $   faix
                                      :++ eRef (cInt 0)
                                      :++ eRef eEmpty
                                      :++ eRef eEmpty
                                      :++ eRef eEmpty
                                      :++ HRN)




-- | Find us a free faix, starting at (deref faixCtr)
allocFaix = mkfdecl $ \_ s -> Decl (Var "allocFaix") (tFun tUnit tfaix) $ DKFunc $
    eLam PUnk $ \_ ->
        eApp (eLam (PRef $ PVar tfaix)
                   (\fa -> eBlock [ eAssign cr (fa `eAdd` cInt 1) ]
                                  $ eITE (eEmpty `eEq` eSlice (PVar fa,PUnk,PUnk)
                                                              (declVar fatab))
                                         (fa)
                                         (eApp s cUnit)))
             cr
 where cr = declVar faixCtr

findFaix = mkdecl $ \_ -> Decl (Var "findFaix") (tFun (tTuple2 (tString, tInt)) tfaix) $ DKFunc $
  eLam (autopv, autopv) $ \(f,n) -> localVar (search f n) (\sr -> emptyPeek sr (create f n) id)
 where
  search f n = eMap (eLam (PVar tfaix,PUnk,PUnk) (\(x,_,_) -> x))
             $ eSlice (PUnk, PVar f, PVar n) (declVar fatab)
  create f n = localVar (eApp (declVar allocFaix) cUnit)
                        (\i -> eBlock [eInsert (declVar fatab) (eTuple3 (i,f,n))] i)
 

allocTermixHelper = mkfdecl $ \_ s ->
  Decl (Var "allocTermixHelper")
       (tFun (tTuple2 (tRef ttermix, tColl CTSet $ tTuple3 (ttermix, targix, indty)))
             ttermix)
  $ DKFunc $
  eLam (autopv,autopv) $ \(tr,ts) -> localVar (deref tr)
                                   $ \ti -> eBlock [ eAssign tr (ti `eAdd` cInt 1) ]
                                                   $ eITE (eEmpty `eEq` eSlice (PVar ti, PUnk, PUnk) ts)
                                                          ti
                                                          (eApp s $ eTuple2 (tr,ts))

-- | Find us a free termix in a given faix
allocTermix = mkfdecl $ \_ s -> Decl (Var "allocTermix") (tFun tfaix ttermix) $ DKFunc $
    eLam autopv $ \fa ->
        eApp (declVar allocTermixHelper)
             (ePeek $ eMap (eLam (PUnk :+ PVar (tRef ttermix) :+ PVar autoty :+ PUnk :+ PUnk :+ HN) $ \(_:+r:+s:+_) -> eTuple2 (r,deref s))
                    $ eSlice (PVar fa:+PUnk:+PUnk:+PUnk:+PUnk:+HN) (declVar heap))

------------------------------------------------------------------------}}}
-- Generic term storage, metaprogramming heap utility functions         {{{

-- | Extract the indirection table for a given faix
hproindr faix = localVar $ ePeek $
  eMap (eLam (PUnk :+ PUnk :+ PVar autoty :+ PUnk :+ PUnk :+ HN)
             (\(_:+_:+ts:+_:+_:+HN) -> ts))
     $ eSlice (PVar faix :+ PUnk :+ PUnk :+ PUnk :+ PUnk :+ HN)
              (declVar heap)

-- | Extract the parasitism table for a given faix
hpropara faix = localVar $ ePeek $
  eMap (eLam (PUnk :+ PUnk :+ PUnk :+ PVar autoty :+ PUnk :+ HN)
             (\(_:+_:+_:+ps:+_:+HN) -> ps))
     $ eSlice (PVar faix :+ PUnk :+ PUnk :+ PUnk :+ PUnk :+ HN)
              (declVar heap)

-- | Extract the evaluation table for a given faix
hproeval faix = localVar $ ePeek $
  eMap (eLam (PUnk :+ PUnk :+ PUnk :+ PUnk :+ PVar autoty :+ HN)
             (\(_:+_:+_:+_:+es:+HN) -> es))
     $ eSlice (PVar faix :+ PUnk :+ PUnk :+ PUnk :+ PUnk :+ HN)
              (declVar heap)


------------------------------------------------------------------------}}}
