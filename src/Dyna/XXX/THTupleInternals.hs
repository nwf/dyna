---------------------------------------------------------------------------
--  | Template haskell for deriving tuple-handling functions

-- Header material                                                      {{{

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dyna.XXX.THTupleInternals where

import           Control.Monad
import           GHC.Exts (maxTupleSize)
import           Language.Haskell.TH

------------------------------------------------------------------------}}}
-- Utilities for tuple manipulation in TH land                          {{{

  -- XXX
foreachTupleSize f = mapM f [2..10] -- maxTupleSize]

mkNames n = mapM (newName . ("mti" ++) . show) [1..n]

genMap app con var = foldl app con . map var

mkTy  n   = genMap appT (conT $ tupleTypeName n) varT
mkDa  n   = genMap appE (conE $ tupleDataName n) varE
mkRTy n r = genMap appT (conT $ tupleTypeName n) (appT r . varT)
mkPTy n   = genMap appT (promotedTupleT n) varT

------------------------------------------------------------------------}}}
-- Make Tuple                                                           {{{

mkTupleInstance :: Name -> Name -> Name -> Name -> Int -> Q Dec
mkTupleInstance _tc _rter _opr _oprs n | n > 1 = do
    -- Build polymorphic variables
  names <- mkNames n

    -- Derive the tuple type and data forms
  let ty = mkTy n names

    -- The constructor and function argument
  vr <- liftM varT $ newName "r"
  vf <- newName "f"

    -- Derive the rtuple type
  let rty = mkRTy n vr names

    -- Patterns and expressions
  let fnames = map (appE (varE vf) . varE) names
  let rpa = tupP $ map varP names
  let frpa = foldl appE (conE $ tupleDataName n) fnames

  instanceD (cxt []) (appT (conT _tc) ty) -- where
            [tySynInstD _rter [ty, vr] rty
            ,funD _opr  [clause [varP vf, rpa] (normalB $ frpa) [] ]
            ,funD _oprs [clause [varP vf, rpa] (normalB $ frpa) [] ]
            ]

mkTupleInstances a b c d = foreachTupleSize (mkTupleInstance a b c d)

------------------------------------------------------------------------}}}
-- Make RTuple                                                          {{{

mkRTupleInstance :: Name -> Name -> Name -> Name -> Int -> Q Dec
mkRTupleInstance _tc _rte _rtr _opel n | n > 1 = do
  names <- mkNames n
  vr <- liftM varT $ newName "r"
  vf <- newName "f"
  let rty = mkRTy n vr names

  let fnames = map (appE (varE vf) . varE) names
  let rpa = tupP $ map varP names
  let lfrpa = listE fnames
  instanceD (cxt []) (appT (conT _tc) rty) -- where
            [tySynInstD _rtr [rty] vr
            ,tySynInstD _rte [rty] $ mkTy n names
            ,funD _opel [clause [varP vf, rpa] (normalB $ lfrpa) [] ]
            ]

mkRTupleInstances a b c d = foreachTupleSize (mkRTupleInstance a b c d)

------------------------------------------------------------------------}}}
-- Make recursive type-math classes which walk tuple types              {{{

  -- XXX TUPLES Can't yet generate the closed lifted-ADTs we use
  -- for class heads.

mkRecClass :: (Name, [TypeQ])       -- ^ Class name and threaded arguments
           -> (Int -> Name)         -- ^ Class argument maker
           -> [(Name,Int -> Name)]  -- ^ Datas and constructor-maker
           -> [Name]                -- ^ Types
           -> [Name]                -- ^ Types with constructor argument
           -> Int                   -- ^ Tuple size
           -> Q Dec
mkRecClass (_cname,_cargs) _ntyf _dnames _tnames _trnames n = do
  names <- mkNames n
  let _tyf = _ntyf n
  let context = cxt $ map (\na -> classP _cname $ _cargs ++ [varT na]) names
  let conarg = (appT (conT _tyf) $ mkPTy n names)

  let datas :: [DecQ]
      datas = map (\(tc,ndc) -> dataInstD (cxt []) tc [conarg]
                     [normalC (ndc n) $ map (strictType (return NotStrict)
                                       . appT (conT tc) . varT)
                                      names]
                     [])
                  $ _dnames

  let types = map (\tc -> tySynInstD tc [conarg]
                   $ genMap appT (conT $ tupleTypeName n)
                     (appT (conT tc) . varT)
                     names)
                  _tnames

  vr <- liftM varT $ newName "r"
  let rtypes = map (\tc -> tySynInstD tc ([conarg] ++ [vr])
                    $ genMap appT (conT $ tupleTypeName n)
                      (\na -> appT (appT (conT tc) (varT na)) vr)
                      names)
                   _trnames

  instanceD context
            (genMap appT (conT _cname) (id) $ _cargs ++ [conarg])
          $ concat [datas,types,rtypes]

------------------------------------------------------------------------}}}
-- Experimental detritus (XXX)                                          {{{

{-
mkNpleFunction :: String -> TypeQ -> Int -> TypeQ
mkNpleFunction _pfx rt n = do
  names <- mkNames n
  let  ty = mkTy n names
  let rty = mkRty n names rt

-}

------------------------------------------------------------------------}}}

