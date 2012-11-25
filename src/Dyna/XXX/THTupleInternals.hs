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

mkNames' pfx n = mapM (newName . (pfx ++) . show) [1..n]

mkNames = mkNames' "mti"

genMap app con var = foldl app con . map var

mkTy  n   = genMap appT (conT $ tupleTypeName n) varT
mkDa  n   = genMap appE (conE $ tupleDataName n) varE
mkRTy n r = genMap appT (conT $ tupleTypeName n) (appT r . varT)
mkPTy n   = genMap appT (promotedTupleT n) varT

pcp x xs = promotedConsT `appT` x `appT` xs
promoteList = foldr pcp promotedNilT

------------------------------------------------------------------------}}}
-- Make MKLT and MKRLT                                                  {{{

mkMKLT :: Name -> Int -> Q Dec
mkMKLT _mklt n = do
  names <- mkNames n

  tySynInstD _mklt [promoteList $ map varT names]
                 $ mkTy n names

mkMKLTs t = foreachTupleSize (mkMKLT t)

mkMKRLT :: Name -> Int -> Q Dec
mkMKRLT _mklrt n = do
  names <- mkNames n
  vr <- liftM varT $ newName "r"

  tySynInstD _mklrt [vr, promoteList $ map varT names]
                  $ mkRTy n vr names

mkMKRLTs t = foreachTupleSize (mkMKRLT t)

------------------------------------------------------------------------}}}
-- Make type-level map functions                                        {{{

mkTyMap :: Int -> Name -> Name -> Q [Dec]
mkTyMap nargs _ty _fn = do
  let ty = conT _ty
  let fn = conT _fn

  args <- liftM (map varT) $ mkNames nargs

  nil  <- tySynInstD _ty (args++[promotedNilT]) promotedNilT

  x  <- liftM varT $ newName "x"
  xs <- liftM varT $ newName "xs"
  let afn = genMap appT fn id args
  let aty = genMap appT ty id args

  cons <- tySynInstD _ty (args++[pcp x xs]) $ pcp (afn `appT` x) (aty `appT` xs)
  return [nil,cons]

mkTyMapFlatN :: Int -> Name -> Name -> Int -> Q Dec
mkTyMapFlatN nargs _ty _fn size = do
  let ty = conT _ty
  let fn = conT _fn
  names <- mkNames size 

  args <- liftM (map varT) $ mkNames nargs
  let afn = genMap appT fn id args

  tySynInstD _ty (args++[promoteList $ map varT names])
               $ genMap appT (conT $ tupleTypeName size)
                 (appT afn . varT)
                 names

-- | The composition mkTyMap (MKLT a)
mkTyMapFlat a b c = foreachTupleSize (mkTyMapFlatN a b c)

mkTyUnMapN :: Maybe Name -> Int -> Name -> Name -> Int -> Q Dec
mkTyUnMapN _mwr nargs _ty _fn size = do
  let ty = conT _ty
  let fn = conT _fn
  names <- mkNames size 

  args <- liftM (map varT) $ mkNames nargs
  let afn = genMap appT fn id args

  tySynInstD _ty (args++[mkTy size names])
                 $ maybe id (\_wr -> appT (conT _wr)) _mwr
                 $ promoteList $ map (appT afn . varT) names

mkTyUnMap a b c d = foreachTupleSize (mkTyUnMapN a b c d)

------------------------------------------------------------------------}}}
-- Make Tuple                                                           {{{

mkTupleInstance _tc _rter _tol _hc _hn _thl _lth _opr _oprs n | n > 1 = do
    -- Build polymorphic variables
  names <- mkNames n

    -- Derive the tuple type and data forms
  let ty = mkTy n names

    -- The constructor and function argument
  vr <- liftM varT $ newName "r"
  f <- newName "f"

    -- Patterns and expressions
  let fnames = map (appE (varE f) . varE) names
  let rpa = tupP $ map varP names
  let frpa = foldl appE (conE $ tupleDataName n) fnames
  let lrpa = foldr (\e l -> (conE _hc) `appE` e `appE` l)
                   (conE _hn)
                   $ map varE names
  let plpa = foldr (\e l -> conP _hc [e,l])
                   (conP _hn [])
                   $ map varP names

  instanceD (cxt []) (appT (conT _tc) ty) -- where
            [tySynInstD _rter [ty, vr] $ mkRTy n vr names
            ,tySynInstD _tol [ty] $ promoteList $ map varT names
            ,funD _thl  [clause [rpa] (normalB $ lrpa) [] ]
            ,funD _lth  [clause [plpa] (normalB $ tupE $ map varE names) [] ]
            ,funD _opr  [clause [varP f, rpa] (normalB $ frpa) [] ]
            ,funD _oprs [clause [varP f, rpa] (normalB $ frpa) [] ]
            ]

mkTupleInstances a b c d e f g h i =
  foreachTupleSize (mkTupleInstance a b c d e f g h i)

------------------------------------------------------------------------}}}
-- Make RTuple                                                          {{{

mkRTupleInstance :: Name -> Name -> Name -> Name -> Int -> Q Dec
mkRTupleInstance _tc _rte _rtr _opel n | n > 1 = do
  names <- mkNames n

    -- i.e. "r :: * -> *"
  vr <- fmap (flip sigT (ArrowT `AppT` StarT `AppT` StarT) . varT)
        $ newName "r"
  f <- newName "f"
  let rty = mkRTy n vr names

  let fnames = map (appE (varE f) . varE) names
  let rpa = tupP $ map varP names
  let lfrpa = listE fnames
  instanceD (cxt []) (appT (conT _tc) rty) -- where
            [tySynInstD _rtr [rty] vr
            ,tySynInstD _rte [rty] $ mkTy n names
            ,funD _opel [clause [varP f, rpa] (normalB $ lfrpa) [] ]
            ]

mkRTupleInstances a b c d = foreachTupleSize (mkRTupleInstance a b c d)

------------------------------------------------------------------------}}}
-- Make recursive type-math classes which walk tuple types (XXX)        {{{

mkTupleRecInstance :: Name         -- ^ Class name
                   -> [TypeQ]      -- ^ Threaded arguments  
                   -> Int          -- ^ Tuple size
                   -> Q Dec
mkTupleRecInstance _cname _cargs n = do
  names <- mkNames n
  let context = cxt $ map (\na -> classP _cname $ _cargs ++ [varT na]) names

  instanceD context
            (genMap appT (conT _cname) (id) $ _cargs ++ [mkTy n names])
            []

mkTupleRecInstances a b = foreachTupleSize (mkTupleRecInstance a b)


{-
mkRecInstance :: (Name, [TypeQ])       -- ^ Class name and threaded arguments
              -> (Int -> Name)         -- ^ Instance argument maker
              -> [(Name,Int -> Name)]  -- ^ Datas and constructor-maker
              -> [Name]                -- ^ Types
              -> [Name]                -- ^ Types with constructor argument
              -> Int                   -- ^ Tuple size
              -> Q Dec
mkRecInstance (_cname,_cargs) _ntyf _dnames _tnames _trnames n = do
  names <- mkNames n
  let _tyf = _ntyf n
  let context = cxt $ map (\na -> classP _cname $ _cargs ++ [varT na]) names
  let conarg = (appT (conT _tyf) $ mkPTy n names)

  let datas = map (\(tc,ndc) -> dataInstD (cxt []) tc [conarg]
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
  let rtypes = map (\tc -> tySynInstD tc (vr:conarg:[])
                    $ genMap appT (conT $ tupleTypeName n)
                      (appT (appT (conT tc) vr) . varT)
                      names)
                   _trnames

  instanceD context
            (genMap appT (conT _cname) (id) $ _cargs ++ [conarg])
          $ concat [datas,types,rtypes]

mkRecInstances a b c d e = foreachTupleSize (mkRecInstance a b c d e)
-}

mkLRecInstance :: (Name, [TypeQ])              -- ^ Class name and args
               -> Name
               -> (Name, Name, Maybe Name, [ExpQ] -> ExpQ)
                                               -- ^ Function, recursive
                                               -- function, data, and body
               -> Int                          -- ^ Tuple size
               -> Q Dec
mkLRecInstance (_cname,_cargs) _tyf (_fn,_rfn,_mfpn,fm) n = do
  names <- mkNames n
  let context = cxt $ map (\na -> classP _cname $ _cargs ++ [varT na]) names
  let conarg = (appT (conT _tyf) $ promoteList $ map varT names)

  resnames <- mkNames n

  let stmts = zipWith (\a ra -> bindS (varP ra) (appE (varE _fn) (varE a)))
              names resnames
  let res = noBindS $ appE (varE $ mkName "return") $ fm $ map varE resnames

  instanceD context (genMap appT (conT _cname) (id) $ _cargs ++ [conarg])
            [funD _fn [clause [maybe tupP conP _mfpn $ [tupP $ map varP names]]
                              (normalB $ doE $ stmts++[res])
                              []
                      ]
            ]

mkLRecInstances a b c = foreachTupleSize (mkLRecInstance a b c)

------------------------------------------------------------------------}}}
-- Experimental detritus (XXX)                                          {{{

-- Tuple-folding/mapping function generation                            

{-
-- | Monadic Tuple Fold and Map
mkMTFoldMap :: Name               -- ^ Function name

            -> Maybe Name         -- ^ Constructor, if any,
                                  -- to be pattern matched away
                                  
            -> ExpQ               -- ^ Mapping applied at each element
                                  --   Should be @a -> m b@

            -> Maybe (ExpQ, ExpQ) -- ^ Common extractor and folder.
                                  --   Should be @(b -> c, [c] -> m d)@
                                  
            -> Maybe ExpQ         -- ^ Map extractor.

            -> Int                -- ^ Tuple size
            -> Q Clause
mkMTFoldMap fn mc m mcf mm n = do
  tnames <- mkNames' "t" n
  rnames <- mkNames' "r" n
  mnames <- mkNames' "m" n

  -- Build the pattern to extract arguments to tnames
  let pat = maybe (id) (\na t -> conP na [t]) mc $
            tupP $ map varP tnames

  -- Apply m
  rstmts <- mapM (\(te,re) -> bindS re $ appE m te) $ zip tnames rnames

  -- Build folding result
  let mfstmt = (\(e,f) -> appE f $ listE $ map (appE e . varE) rnames)
               `fmap` mcf

  -- Build mapping result
  let mmstmt = (\m -> genMap appE (conE $ tupleDataName n)
                      $ map (appE m . varE) rnames)
               `fmap` mm

  let pfx = rstmts
  
  clause pat (normalB $ doE pfx) []
-}


{-
mkNpleFunction :: String -> TypeQ -> Int -> TypeQ
mkNpleFunction _pfx rt n = do
  names <- mkNames n
  let  ty = mkTy n names
  let rty = mkRty n names rt

-}

------------------------------------------------------------------------}}}

