---------------------------------------------------------------------------
--   | Provides the "AsK3" type and instances for the K3 AST.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.BackendK3.Render (
    -- * K3 implementations
    AsK3Ty(..), AsK3(..),

    -- * Utility functions
    sh, sht, shd
) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List              as DL
import           Text.PrettyPrint.Free

import           Dyna.BackendK3.AST
import           Dyna.XXX.HList
import           Dyna.XXX.MonadUtils
import           Dyna.XXX.THTuple

import qualified Language.Haskell.TH    as TH


------------------------------------------------------------------------}}}
-- Collection handling                                                  {{{

class K3CFn (c :: CKind) where
  k3cfn_empty :: AsK3 e (CTE (AsK3 e) c a)
  k3cfn_sing  :: AsK3 e vma -> AsK3 e (CTE (AsK3 e) c vma)

instance K3CFn CSet where
  k3cfn_empty = AsK3$ const$ "{ }"
  k3cfn_sing (AsK3 e) = AsK3$ braces . e

instance K3CFn CList where
  k3cfn_empty = AsK3$ const$ "[ ]"
  k3cfn_sing (AsK3 e) = AsK3$ brackets . e

instance K3CFn CBag where
  k3cfn_empty = AsK3$ const$ "{| |}"
  k3cfn_sing (AsK3 e) = AsK3$ encBag . e

------------------------------------------------------------------------}}}
-- Pattern handling                                                     {{{

class (Pat UnivTyRepr w) => K3PFn w where
  -- | Turn a pattern into two parts: the string to be placed after the
  -- \ in the K3 code and the constitutent pieces to be passed into the
  -- HOAS function given to eLam
  k3pfn :: PatDa w -> ReaderT Bool (State Int) (Doc e, PatReprFn (AsK3 e) w)

rec_k3pfn :: (K3PFn w)
          => PatDa w
          -> ReaderT Bool (State Int) (Doc e, PatReprFn (AsK3 e) w)
rec_k3pfn = local (const False) . k3pfn

instance (K3BaseTy a) => K3PFn (PKVar UnivTyRepr (a :: *)) where
  k3pfn (PVar (tr :: UnivTyRepr a)) = do
    n <- lift incState
    let sn = text $ "x" ++ show n
    return (sn <> colon <> unAsK3Ty (unUTR tr)
           ,AsK3$ const$ sn)

instance (K3BaseTy a) => K3PFn (PKUnk (a :: *)) where
  k3pfn PUnk = return ("_", ())

instance (K3PFn w) => K3PFn (PKJust w) where
  k3pfn (PJust w) = do
    (p,r) <- rec_k3pfn w
    return ("just" <+> parens p, r)

instance (K3PFn w) => K3PFn (PKRef w) where
  k3pfn (PRef w) = rec_k3pfn w

instance K3PFn (PKHL '[]) where
  k3pfn HN = ask >>= \f -> return (if f then "" else "()", HN)

instance (K3PFn w, K3PFn (PKHL ws), MapPatConst ws UnivTyRepr)
      => K3PFn (PKHL (w ': ws)) where
  k3pfn (w :+ ws) = do
        (pw,rw) <- k3pfn w
        (ps,rs) <- local (const True) $ k3pfn ws
        p <- asks (\f -> (if f then (comma <>) else parens) (pw <> ps))
        let r = rw :+ rs
        return (p,r)


------------------------------------------------------------------------}}}
-- Slice handling                                                       {{{

class (Pat (AsK3 e) w) => K3SFn e w where
  -- | Print a pattern
  k3sfn :: PatDa w -> Reader Bool (AsK3 e (PatTy (AsK3 e) w))

rec_k3sfn :: (K3SFn e w)
          => PatDa w
          -> Reader Bool (AsK3 e (PatTy (AsK3 e) w))
rec_k3sfn = local (const False) . k3sfn

instance (K3BaseTy a) => K3SFn e (PKVar (AsK3 e) (a :: *)) where
  k3sfn (PVar r) = return r

instance (K3BaseTy a) => K3SFn e (PKUnk (a :: *)) where
  k3sfn PUnk = return $ AsK3$ const$ text "_"

instance (K3SFn e s) => K3SFn e (PKJust s) where
  k3sfn (PJust s) = do
    p <- rec_k3sfn s
    return $ AsK3$ \n -> "just" <> parens (unAsK3 p n)

instance K3SFn e (PKHL '[]) where
  k3sfn HN = asks (\f -> AsK3$ const$ if f then "" else "()")

instance (K3SFn e w, K3SFn e (PKHL ws), MapPatConst ws (AsK3 e))
      => K3SFn e (PKHL (w ': ws)) where
  k3sfn (w :+ ws) = do
        pw <- k3sfn w
        ps <- local (const True) $ k3sfn ws
        fn <- asks (\f -> (if f then (comma <>) else parens))
        return$ AsK3$ \n -> fn $ (unAsK3 pw n) <> (unAsK3 ps n)

------------------------------------------------------------------------}}}
{- * Annotations -} --                                                  {{{

fdscast :: FunDepSpec a -> FunDepSpec b
fdscast FDIrr = FDIrr
fdscast FDDom = FDDom
fdscast FDCod = FDCod

annfds :: (RTupled fs, RTR fs ~ FunDepSpec)
       => Doc e -> fs -> Doc e
annfds op fs =
  let x = tupleopEL (fdscast) fs
      (dom,cod) = (DL.elemIndices FDDom x 
                  ,DL.elemIndices FDCod x)
  in     (tupled $ map pretty dom)
     <+> op
     <+> (tupled $ map pretty cod)

annfdshl :: Doc e -> HRList FunDepSpec a -> Doc e
annfdshl op fs =
  let x = hrlproj (fdscast) fs
      (dom,cod) = (DL.elemIndices FDDom x 
                  ,DL.elemIndices FDCod x)
  in     (tupled $ map pretty dom)
     <+> op
     <+> (tupled $ map pretty cod)


annText :: Ann a -> Doc e
annText  AAtomic       = "atomic"
annText (AFunDep fs)   = annfds   "->" fs
annText (AFunDepHL fs) = annfdshl "->" fs
annText (AIndex fs)    = annfds   "=>" fs
annText (AIndexHL fs)  = annfdshl "=>" fs
annText  AOneOf        = "oneof"
annText  AOneOfHL      = "oneof"
annText (AMisc s)      = text s

------------------------------------------------------------------------}}}
{- * Type handling -} --                                                {{{

-- | Produce a textual representation of a K3 type
--
--   Unlike AsK3 below, we don't need to thread a variable counter
--   around since K3 doesn't have tyvars
newtype AsK3Ty e (a :: *) = AsK3Ty { unAsK3Ty :: Doc e }

instance K3Ty (AsK3Ty e) where
  tAnn (AsK3Ty e) anns = AsK3Ty$ align $
       e </> "@" <+> (encloseSep lbrace rbrace comma $ map annText anns)

  tAddress = AsK3Ty$ "address"
  tBool    = AsK3Ty$ "bool"
  tByte    = AsK3Ty$ "byte"
  tFloat   = AsK3Ty$ "float"
  tInt     = AsK3Ty$ "int"
  tString  = AsK3Ty$ "string"
  tUnit    = AsK3Ty$ "unit"

  -- tPair (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ tupled [ ta, tb ]

  tMaybe (AsK3Ty ta) = AsK3Ty$ "Maybe" <+> ta

  tColl CTSet  (AsK3Ty ta) = AsK3Ty$ braces   ta
  tColl CTBag  (AsK3Ty ta) = AsK3Ty$ encBag   ta
  tColl CTList (AsK3Ty ta) = AsK3Ty$ brackets ta

  tFun (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ ta `above` ("->" <+> tb)

  tRef (AsK3Ty ta) = AsK3Ty$ "ref" <+> ta

    -- XXX TUPLES Note the similarities!
  tTuple2 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple3 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple4 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple5 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us

  tHL     us = AsK3Ty $ tupled $ hrlproj unAsK3Ty us

------------------------------------------------------------------------}}}
-- Expression handling                                                  {{{

-- | Produce a textual representation of a K3 expression
newtype AsK3 e (a :: *) = AsK3 { unAsK3 :: Int -> Doc e }

type instance K3_Coll_C (AsK3 e) c = K3CFn c
type instance K3_Pat_C (AsK3 e) p = K3PFn p
type instance K3_Slice_C (AsK3 e) s = K3SFn e s

instance K3 (AsK3 e) where

  cAnn (AsK3 e) anns = AsK3$ \n -> align $
       parens (e n) <> " @ "
    <> (encloseSep lbrace rbrace comma $ map annText anns)

  cComment str (AsK3 a) = AsK3$ \n -> "\n// " <> text str <> "\n" <> a n

  cAddress (Addr (h,p)) = AsK3$ const$ (text h <> ":" <> pretty p)

  cBool    n     = AsK3$ const$ text$ show n
  cByte    n     = AsK3$ const$ text$ show n
  cFloat   n     = AsK3$ const$ text$ show n
  cInt     n     = AsK3$ const$ text$ show n
  cString  n     = AsK3$ const$ text$ show n
  cNothing       = AsK3$ const$ "nothing"
  cUnit          = AsK3$ const$ "unit"

  unsafeVar (Var v) _ = AsK3$ const$ text v


  eJust (AsK3 a)          = builtin "just" [ a ]
  eRef  (AsK3 a)          = builtin "ref" [ a ]

    -- XXX TUPLES Note the similarity of these!
  eTuple2 t = AsK3 $ \n -> tupled $ tupleopEL (flip unAsK3 n) t
  eTuple3 t = AsK3 $ \n -> tupled $ tupleopEL (flip unAsK3 n) t
  eTuple4 t = AsK3 $ \n -> tupled $ tupleopEL (flip unAsK3 n) t
  eTuple5 t = AsK3 $ \n -> tupled $ tupleopEL (flip unAsK3 n) t

  eHL     t = AsK3 $ \n -> tupled $ hrlproj   (flip unAsK3 n) t

  eEmpty = k3cfn_empty
  eSing  = k3cfn_sing
  eCombine (AsK3 a) (AsK3 b) = AsK3$ \n -> parens (a n) <> " ++ " <> parens (b n)
  eRange (AsK3 f) (AsK3 l) (AsK3 s) = builtin "range" [ f, l, s ]
  
  eAdd = binop "+"
  eMul = binop "*"
  eNeg (AsK3 b) = AsK3$ \n -> "-" <> parens (b n)

  eEq  = binop "=="
  eLt  = binop "<"
  eLeq = binop "<="
  eNeq = binop "!="

  eLam w f = AsK3$ \n -> let ((pat, arg),n') = runState (runReaderT (k3pfn w) False) n
                         in "\\" <> pat <+> "->" `above` indent 2 (unAsK3 (f arg) n')

  eApp (AsK3 f) (AsK3 x) = AsK3$ \n ->
    parens (parens (f n) `aboveBreak` parens (x n))

  eBlock ss (AsK3 r) = AsK3$ \n -> 
    "do" <> (semiBraces (map ($ n) ((map unAsK3 ss) ++ [r])))

  eIter (AsK3 f) (AsK3 c) = builtin "iterate" [ f, c ]

  eITE (AsK3 b) (AsK3 t) (AsK3 e) = AsK3$ \n ->
    "if" <+> (align $ above (parens (b n))
                            ("then" <+> parens (t n) `aboveBreak`
                             "else"  <+> parens (e n)))

  eMap     (AsK3 f) (AsK3 c)                   = builtin "map"       [ f, c    ]
  eFiltMap (AsK3 f) (AsK3 m) (AsK3 c)          = builtin "filtermap" [ f, m, c ]
  eFlatten (AsK3 c)                            = builtin "flatten"   [ c ]
  eFold    (AsK3 f) (AsK3 z) (AsK3 c)          = builtin "fold"      [ f, z, c ]
  eGBA     (AsK3 p) (AsK3 f) (AsK3 z) (AsK3 c) = builtin "groupby"   [ p, f, z, c ]
  eSort    (AsK3 c) (AsK3 f)                   = builtin "sort"      [ c, f ]
  ePeek    (AsK3 c)                            = builtin "peek"      [ c ]

  eSlice w (AsK3 c) = AsK3$ \n -> c n <> brackets (unAsK3 (runReader (k3sfn w) False) n)

  eInsert (AsK3 c) (AsK3 e)          = builtin "insert" [ c, e ]
  eDelete (AsK3 c) (AsK3 e)          = builtin "delete" [ c, e ]
  eUpdate (AsK3 c) (AsK3 o) (AsK3 n) = builtin "update" [ c, o, n ]

  eAssign          = binop "<-" 
  
  eSend (AsK3 a) (AsK3 f) (AsK3 x) = builtin "send" [ a, f, x ] 

------------------------------------------------------------------------}}}
-- Miscellany                                                           {{{

encBag :: Doc e -> Doc e
encBag = enclose "{|" "|}"

    -- Overly polymorphic; use only when correct!
binop :: Doc e -> AsK3 e a -> AsK3 e b -> AsK3 e c
binop o (AsK3 a) (AsK3 b) = AsK3$ \n ->     parens (align $ a n)
                                        </> o
                                        <+> parens (align $ b n)

    -- Overly polymorphic; use only when correct!
builtin :: Doc e -> [ Int -> Doc e ] -> AsK3 e b
builtin fn as = AsK3$ \n -> fn <> tupled (map ($ n) as)

instance Show (AsK3 e a) where
  show (AsK3 f) = show $ f 0

sh :: AsK3 e a -> Doc e
sh f = unAsK3 f 0

instance Show (AsK3Ty e a) where
  show (AsK3Ty f) = show f

sht :: AsK3Ty e a -> Doc e
sht = unAsK3Ty

shd :: Decl (AsK3Ty e) (AsK3 e) t -> Doc e
shd (Decl (Var name) tipe body) =
     "declare "
  <> text name
  <+> align (colon <+> unAsK3Ty tipe)
  <> case body of
       Nothing -> empty
       Just b  -> space <> equals `aboveBreak` (indent 2 $ unAsK3 b 0)
  <> semi

------------------------------------------------------------------------}}}
-- Template Haskell splices                                             {{{

$(mkLRecInstances (''K3PFn,[]) 'PKTup 
                  ('k3pfn,'rec_k3pfn,Nothing,\ls -> TH.tupE [
                                          TH.appE (TH.varE 'tupled)
                                          $ TH.listE
                                          $ map (TH.appE (TH.varE 'fst)) ls
                                       , TH.tupE $ map (TH.appE (TH.varE 'snd)) ls
                                       ]
                  ))

$(do
    e <- liftM TH.varT $ TH.newName "e"
    n <- TH.newName "n"
    mkLRecInstances (''K3SFn,[e]) 'PKTup 
                  ('k3sfn,'rec_k3sfn,Nothing,\ls ->
                      TH.appE (TH.conE 'AsK3)
                    $ TH.lamE [TH.varP n]
                    $ TH.appE (TH.varE 'tupled)
                    $ TH.listE
                    $ map (\l -> TH.appE (TH.appE (TH.varE 'unAsK3) l) (TH.varE n))
                    $ ls
                  ))
