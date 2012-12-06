---------------------------------------------------------------------------
-- | Print a K3 AST or Type in a way that the K3 compiler understands.
--
-- XXX Note that the output is currently hideously ugly.  We really should
-- fix that.

-- Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Dyna.BackendK3.Render (
    -- * K3 implementations
    AsK3Ty(..), AsK3E(..),

    -- * Utility functions
    sh, sht, shd, shk3
) where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.List              as DL
import qualified Data.Map               as M
import           Text.PrettyPrint.Free

import           Dyna.BackendK3.AST
import           Dyna.BackendK3.CollectDecls
import           Dyna.XXX.HList
import           Dyna.XXX.MonadUtils
import           Dyna.XXX.THTuple

-- import qualified Language.Haskell.TH    as TH


------------------------------------------------------------------------}}}
-- Collection handling                                                  {{{

k3cfn_empty :: CollTy c -> AsK3E e (CTE (AsK3E e) c a)
k3cfn_empty CSet  = AsK3E$ const$ "{ }"
k3cfn_empty CList = AsK3E$ const$ "[ ]"
k3cfn_empty CBag  = AsK3E$ const$ "{| |}"

k3cfn_sing  :: CollTy c -> AsK3E e vma -> AsK3E e (CTE (AsK3E e) c vma)
k3cfn_sing CSet  (AsK3E e) = AsK3E$ braces . e
k3cfn_sing CList (AsK3E e) = AsK3E$ brackets . e
k3cfn_sing CBag  (AsK3E e) = AsK3E$ encBag . e

------------------------------------------------------------------------}}}
-- Pattern handling                                                     {{{

rec_k3pfn :: PDat UnivTyRepr w
          -> ReaderT Bool (State Int) (Doc e, PatReprFn (AsK3E e) w)
rec_k3pfn = local (const False) . k3pfn

-- | Turn a pattern into two parts: the string to be placed after the
-- \ in the K3 code and the constitutent pieces to be passed into the
-- HOAS function given to eLam
k3pfn :: PDat UnivTyRepr w -> ReaderT Bool (State Int) (Doc e, PatReprFn (AsK3E e) w)
k3pfn (PVar tr) = do
    n <- lift incState
    let sn = text $ "x" ++ show n
    return (sn <> colon <> unAsK3Ty (unUTR tr)
           ,AsK3E$ const$ sn)
k3pfn PUnk = return ("_", ())
k3pfn (PJust w) = rec_k3pfn w >>= \(p,r) -> return ("just" <+> parens p, r)
-- XXX k3ref
-- k3pfn (PRef w) = rec_k3pfn w
k3pfn (PHL HRN) = ask >>= \f -> return (if f then "" else "()", HN)
k3pfn (PHL (w :++ ws)) = do
    (pw,rw) <- k3pfn w
    (ps,rs) <- local (const True) $ k3pfn (PHL ws)
    p <- asks (\f -> (if f then (comma <>) else parens) (pw <> ps))
    let r = rw :+ rs
    return (p,r)
k3pfn (PT2 (a,b)) = do
    (pa,ra) <- k3pfn a
    (pb,rb) <- k3pfn b
    let p = tupled [pa,pb]
    let r = (ra,rb)
    return (p,r)
k3pfn (PT3 (a,b,c)) = do
    (pa,ra) <- k3pfn a
    (pb,rb) <- k3pfn b
    (pc,rc) <- k3pfn c
    let p = tupled [pa,pb,pc]
    let r = (ra,rb,rc)
    return (p,r)
k3pfn (PT4 (a,b,c,d)) = do
    (pa,ra) <- k3pfn a
    (pb,rb) <- k3pfn b
    (pc,rc) <- k3pfn c
    (pd,rd) <- k3pfn d
    let p = tupled [pa,pb,pc,pd]
    let r = (ra,rb,rc,rd)
    return (p,r)
k3pfn (PT5 (a,b,c,d,e)) = do
    (pa,ra) <- k3pfn a
    (pb,rb) <- k3pfn b
    (pc,rc) <- k3pfn c
    (pd,rd) <- k3pfn d
    (pe,re) <- k3pfn e
    let p = tupled [pa,pb,pc,pd,pe]
    let r = (ra,rb,rc,rd,re)
    return (p,r)


------------------------------------------------------------------------}}}
-- Slice handling                                                       {{{

rec_k3sfn :: PDat (AsK3E e) w -> Reader Bool (AsK3E e (PatTy (AsK3E e) w))
rec_k3sfn = local (const False) . k3sfn

-- | Print a pattern
k3sfn :: PDat (AsK3E e) w -> Reader Bool (AsK3E e (PatTy (AsK3E e) w))
k3sfn (PVar r) = return r
k3sfn PUnk = return $ AsK3E$ const$ text "_"
k3sfn (PJust s) = do
    p <- rec_k3sfn s
    return $ AsK3E$ \n -> "just" <> parens (unAsK3E p n)
{- XXX k3ref
k3sfn (PRef x) = do
    p <- rec_k3sfn x
    return $ AsK3E$ unAsK3E p  -- coerce
-}
k3sfn (PHL HRN) = asks (\f -> AsK3E$ const$ if f then "" else "()")
k3sfn (PHL (w :++ ws)) = do
    pw <- k3sfn w
    ps <- local (const True) $ k3sfn (PHL ws)
    fn <- asks (\f -> (if f then (comma <>) else parens))
    return$ AsK3E$ \n -> fn $ (unAsK3E pw n) <> (unAsK3E ps n)
k3sfn (PT2 (a,b)) = do
    pa <- k3sfn a
    pb <- k3sfn b
    return$ AsK3E$ \n -> tupled [unAsK3E pa n, unAsK3E pb n]
k3sfn (PT3 (a,b,c)) = do
    pa <- k3sfn a
    pb <- k3sfn b
    pc <- k3sfn c
    return$ AsK3E$ \n -> tupled [unAsK3E pa n, unAsK3E pb n, unAsK3E pc n]
k3sfn (PT4 (a,b,c,d)) = do
    pa <- k3sfn a
    pb <- k3sfn b
    pc <- k3sfn c
    pd <- k3sfn d
    return$ AsK3E$ \n -> tupled [unAsK3E pa n, unAsK3E pb n
                               ,unAsK3E pc n, unAsK3E pd n]
k3sfn (PT5 (a,b,c,d,e)) = do
    pa <- k3sfn a
    pb <- k3sfn b
    pc <- k3sfn c
    pd <- k3sfn d
    pe <- k3sfn e
    return$ AsK3E$ \n -> tupled [unAsK3E pa n, unAsK3E pb n
                               ,unAsK3E pc n, unAsK3E pd n
                               ,unAsK3E pe n]


------------------------------------------------------------------------}}}
-- Annotations                                                          {{{

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

{- XXX k3xref
newtype K3RXref a = K3RXR { unK3RXR :: String }

rxref_mk :: PDat r w -> ReaderT String (State Int) (PatReprFn K3RXref w)
rxref_mk (PVar _) = ask >>= \p -> get >>= \i -> return $ K3RXR (p <> show i)
rxref_mk PUnk = return ()
{- XXX k3ref
rxref_mk (PRef w) = do
    pfx <- ask
    ix  <- get
    (r,_) <- local (const $ pfx <> show ix) $ bracketState 0 $ rxref_mk w
    return r

-- test (AXref pd fn _ _) = unK3RXR $ fn (evalState (runReaderT (rxref_mk pd) "x") 0)
-}

{-
 - Some older code for doing this; this has obsolesced but may be useful in
 - writing the above.
class (UnPatDa (PatDa w) ~ w) => RXref (w :: PKind) where

instance (K3BaseTy a) => RXref (PKVar (r :: * -> *) (a :: *)) where

instance (K3BaseTy a) => RXref (PKUnk (a :: *)) where

instance (RXref w) => RXref (PKRef w) where

instance (UnMapPatDa (HList (MapPatDa ws)) ~ ws,
          MapPatConst ws K3RXref,
          MapConstraint RXref ws)
      => RXref (PKHL '[]) where
  rxref_mk HN = return HN

instance (RXref w, RXref (PKHL ws),
          MapPatConst ws K3RXref)
      => RXref (PKHL (w ': ws)) where
  rxref_mk (w :+ ws) = do
    pfx <- ask
    ix  <- incState
    (rw,_) <- local (const $ pfx <> show ix) $ bracketState 0 $ rxref_mk w
    rs <- rxref_mk ws
    return (rw :+ rs)
-}
-}

annTText :: AnnT a -> Doc e
annTText (AFunDep fs)             = annfds   "->" fs
annTText (AFunDepHL fs)           = annfdshl "->" fs
annTText (AIndex _)               = "" -- XXX not yet supported annfds   "=>" fs
annTText (AIndexHL _)             = "" -- XXX not yet supported annfdshl "=>" fs
annTText  AOneOf                  = "" -- XXX not yet supported "oneof"
annTText  AOneOfHL                = "" -- XXX not yet supported "oneof"
annTText (ATMisc s)               = text s
-- XXX k3xref
-- annTText (AXref _ _ _ _ _)        = "" -- XXX
-- annTText (AXrefF _ _ _ _ _ _)     = "" -- XXX

annEText :: AnnE a -> Doc e
annEText  AAtomic       = "atomic"
annEText  ASingleton    = "singleton"
annEText (AEMisc s)     = text s

------------------------------------------------------------------------}}}
-- Type handling                                                        {{{

-- | Produce a textual representation of a K3 type
--
--   Unlike AsK3E below, we don't need to thread a variable counter
--   around since K3 doesn't have tyvars
newtype AsK3Ty e (a :: *) = AsK3Ty { unAsK3Ty :: Doc e }

instance K3Ty (AsK3Ty e) where
  tAnn (AsK3Ty e) anns = AsK3Ty$ align $
       e </> "@" <+> (encloseSep lbrace rbrace comma $ map annTText anns)

  tAddress = AsK3Ty$ "address"
  tBool    = AsK3Ty$ "bool"
  tByte    = AsK3Ty$ "byte"
  tFloat   = AsK3Ty$ "float"
  tInt     = AsK3Ty$ "int"
  tString  = AsK3Ty$ "string"
  tUnit    = AsK3Ty$ "unit"

  -- XXX is that right?
  tTarget _ = AsK3Ty$ "target"

  -- tPair (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ tupled [ ta, tb ]

  tMaybe (AsK3Ty ta) = AsK3Ty$ "Maybe" <+> ta

  tColl CSet  (AsK3Ty ta) = AsK3Ty$ braces   ta
  tColl CBag  (AsK3Ty ta) = AsK3Ty$ encBag   ta
  tColl CList (AsK3Ty ta) = AsK3Ty$ brackets ta

  tFun (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ ta `above` ("->" <+> tb)

  -- XXX k3ref
  -- tRef (AsK3Ty ta) = AsK3Ty$ "ref" <+> ta

    -- XXX TUPLES Note the similarities!
  tTuple2 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple3 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple4 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us
  tTuple5 us = AsK3Ty $ tupled $ tupleopEL unAsK3Ty us

  tHL     us = AsK3Ty $ tupled $ hrlproj unAsK3Ty us

------------------------------------------------------------------------}}}
-- Expression handling                                                  {{{

data Prec = PrecLowest
          | PrecITE
          | PrecBOComp
          | PrecBOAdd
          | PrecBOMul
          | PrecNeg
          | PrecApp
 deriving (Enum,Eq,Ord,Show)

-- | Produce a textual representation of a K3 expression
newtype AsK3E e (a :: *) = AsK3E { unAsK3E :: (Int,Prec) -> Doc e }

instance K3 (AsK3E e) where
  declVar (Decl (Var v) _)   = AsK3E$ const$ text v
  unsafeVar (Var v) _        = AsK3E$ const$ text v

  cAnn (AsK3E e) anns = AsK3E$ \n -> align $
       parens (e n) <> " @ "
    <> (encloseSep lbrace rbrace comma $ map annEText anns)

  cComment str (AsK3E a) = AsK3E$ \n -> "\n// " <> text str <> "\n" <> a n

  cAddress (Addr (h,p)) = AsK3E$ const$ (text h <> ":" <> pretty p)

  cBool    n     = AsK3E$ const$ text$ show n
  cByte    n     = AsK3E$ const$ text$ show n
  cFloat   n     = AsK3E$ const$ text$ show n
  cInt     n     = AsK3E$ const$ text$ show n
  cString  n     = AsK3E$ const$ text$ show n
  cNothing       = AsK3E$ const$ "nothing"
  cUnit          = AsK3E$ const$ "unit"


  eJust (AsK3E a)          = builtin "just" [ a ]
  -- XXX k3ref
  -- eRef  (AsK3E a)          = builtin "ref" [ a ]

    -- XXX TUPLES Note the similarity of these!
  eTuple2 t = AsK3E $ \(n,_) -> tupled $ tupleopEL (flip unAsK3E (n,PrecLowest)) t
  eTuple3 t = AsK3E $ \(n,_) -> tupled $ tupleopEL (flip unAsK3E (n,PrecLowest)) t
  eTuple4 t = AsK3E $ \(n,_) -> tupled $ tupleopEL (flip unAsK3E (n,PrecLowest)) t
  eTuple5 t = AsK3E $ \(n,_) -> tupled $ tupleopEL (flip unAsK3E (n,PrecLowest)) t

  eHL     t = AsK3E $ \(n,_) -> tupled $ hrlproj   (flip unAsK3E (n,PrecLowest)) t

  eEmpty = k3cfn_empty
  eSing  = k3cfn_sing
  eCombine (AsK3E a) (AsK3E b) = AsK3E$ \n -> parens (a n) <> " ++ " <> parens (b n)
  eRange (AsK3E f) (AsK3E l) (AsK3E s) = builtin "range" [ f, l, s ]
  
  eAdd = binop PrecBOAdd "+"
  eMul = binop PrecBOMul "*"
  eNeg (AsK3E b) = AsK3E$ \(n,p) -> np p PrecNeg $ "-" <> (b (n,PrecNeg))

  eEq  = binop PrecBOComp "=="
  eLt  = binop PrecBOComp "<"
  eLeq = binop PrecBOComp "<="
  eNeq = binop PrecBOComp "!="

  eLam w f = AsK3E$ \(n,_) -> let ((pat, arg),n') = runState (runReaderT (k3pfn w) False) n
                             in "\\" <> pat <+> "->" `above` indent 2 (unAsK3E (f arg) (n',PrecLowest))

  eApp (AsK3E f) (AsK3E x) = AsK3E$ \n ->
    parens (parens (f n) </> parens (x n))

  eBlock ss (AsK3E r) = AsK3E$ \(n,_) -> 
    "do" <> (semiBraces (map ($ (n,PrecLowest)) ((map unAsK3E ss) ++ [r])))

  eIter (AsK3E f) (AsK3E c) = builtin "iterate" [ f, c ]

  eITE (AsK3E b) (AsK3E t) (AsK3E e) = AsK3E$ \(n,p) -> np p PrecITE $
    "if" <+> (align $ above (parens (b (n,PrecLowest)))
                            ("then" <+> t (n,PrecLowest) `aboveBreak`
                             "else"  <+> e (n,PrecLowest)))

  eMap     (AsK3E f) (AsK3E c)                     = builtin "map"       [ f, c    ]
  eFiltMap (AsK3E f) (AsK3E m) (AsK3E c)           = builtin "filtermap" [ f, m, c ]
  eFlatten (AsK3E c)                               = builtin "flatten"   [ c ]
  eFold    (AsK3E f) (AsK3E z) (AsK3E c)           = builtin "fold"      [ f, z, c ]
  eGBA     (AsK3E p) (AsK3E f) (AsK3E z) (AsK3E c) = builtin "groupby"   [ p, f, z, c ]
  eSort    (AsK3E c) (AsK3E f)                     = builtin "sort"      [ c, f ]
  ePeek    (AsK3E c)                               = builtin "peek"      [ c ]

  eSlice w (AsK3E c) = AsK3E$ \n -> c n <> brackets (unAsK3E (runReader (k3sfn w) False) n)

  eInsert (AsK3E c) (AsK3E e)          = builtin "insert" [ c, e ]
  eDelete (AsK3E c) (AsK3E e)          = builtin "delete" [ c, e ]
  eUpdate (AsK3E c) (AsK3E o) (AsK3E n) = builtin "update" [ c, o, n ]

  -- XXX k3ref
  -- eAssign          = binop PrecBOComp "<-" 
  
  eSend (AsK3E a) (AsK3E f) (AsK3E x) = builtin "send" [ a, f, x ] 

inist :: (Int,Prec)
inist = (0,PrecLowest)

encBag :: Doc e -> Doc e
encBag = enclose "{|" "|}"

np :: forall a e. Ord a => a -> a -> Doc e -> Doc e
np p p' = (if p > p' then parens else id)

-- Overly polymorphic; use only when correct!
binop :: Prec -> Doc e -> AsK3E e a -> AsK3E e b -> AsK3E e c
binop p' o (AsK3E a) (AsK3E b) =
  AsK3E$ \(n,p) -> np p p' $ (align $ a (n,p')) </> o <+> (align $ b (n,succ p'))

-- Overly polymorphic; use only when correct!
builtin :: Doc e -> [ (Int,Prec) -> Doc e ] -> AsK3E e b
builtin fn as = AsK3E$ \(n,_) -> fn <> tupled (map ($ (n,PrecLowest)) as)

{-
-- | Since the entire type of an AsK3 is phantom, we can, of course, alter
-- it at a moment's notice.  Note that this is generally probably unwise.
phantom_ask3e :: forall t e a. AsK3E e t -> AsK3E e a
phantom_ask3e (AsK3E f) = AsK3E f

{- XXX k3ref
-- | Shift the phantom type for a reference inside AsK3E's interpretation
phantom_ref :: AsK3E e (Ref (AsK3E e) a) -> AsK3E e (Ref r a)
phantom_ref = phantom_ask3e
-}

-- | Shift the phantom type for a collection inside AsK3E's interpretation
phantom_coll :: AsK3E e (CTE (AsK3E e) c a) -> AsK3E e (CTE r c a)
phantom_coll = phantom_ask3e
-}

instance Show (AsK3E e a) where
  show (AsK3E f) = show $ f inist

sh :: AsK3E e a -> Doc e
sh f = unAsK3E f inist

instance Show (AsK3Ty e a) where
  show (AsK3Ty f) = show f

sht :: AsK3Ty e a -> Doc e
sht = unAsK3Ty

declKeyword :: DBody t -> Doc e
declKeyword (DColl _)    = "declare"
declKeyword (DTrig _)    = "trigger"
-- declKeyword DRef      = "declare"
declKeyword (DFunc _) = "declare"

shdk :: DBody t -> Doc e
shdk d = case d of
  (DColl ty) -> align (colon <+> sht (unUTR ty))
  (DTrig b)  -> renderBody b
  -- XXX k3ref
  -- shdk DRef      = empty
  (DFunc b)  -> renderBody b
 where
  renderBody b = space <> equals `aboveBreak`
                          (indent 2 $ unAsK3E b inist)

shd :: Decl s t -> Doc e
shd (Decl (Var name) {- tipe -} body) =
      declKeyword body
  <+> text name
  <+> shdk body
   <> semi
   <> line

------------------------------------------------------------------------}}}
-- Program handling                                                     {{{

shex :: ExDecl -> Doc e
shex (Ex d) = shd d

shk3 :: Prog -> (Doc e, [Doc e])
shk3 r = (case r of (Prog d) -> shd d,
          case r of (Prog (Decl _ b)) -> map shex $ M.elems $ cdk b)

{-
-- | Produce a textual representation of a K3 program, including all
-- referenced declarations.
--
-- XXX I would rather do this differently, if I can, but for the moment,
-- this suffices.
data AsK3P e (a :: *) = AsK3P { ask3p_exp   :: AsK3E e a
                              , ask3p_decls :: M.Map VarIx (Doc e)
                              }

phantom_pat_prf :: PDat UnivTyRepr w
                -> PatReprFn (AsK3P e) w
                -> PatReprFn (AsK3E e) w
phantom_pat_prf (PVar _) (AsK3P e _) = e

phantom_lam :: PDat UnivTyRepr w
            -> AsK3E e (PatTy (AsK3E e) w -> b)
            -> AsK3E e (PatTy (AsK3P e) w -> b)
phantom_lam _ = phantom_ask3e

mkpc :: Proxy e -> PDat UnivTyRepr w -> PatReprFn (AsK3P e) w
mkpc _ (PVar _) = AsK3P (AsK3E $ const empty) M.empty
mkpc _ PUnk = ()
mkpc p (PJust w) = mkpc p w
-- XXX k3ref
-- mkpc p (PRef w) = mkpc p w
mkpc _ (PHL HRN) = HN
mkpc p (PHL (w :++ ws)) = mkpc p w :+ mkpc p (PHL ws)

instance K3 (AsK3P e) where

  type K3_M (AsK3P e) = K3_M (AsK3E e)

  cComment = flip const
  cAnn     = const

  cAddress a = AsK3P (cAddress a) M.empty
  cBool    a = AsK3P (cBool a)    M.empty
  -- XXX

  eJust  (AsK3P e d) = AsK3P (eJust e) d
  -- XXX k3ref
  -- eRef   (AsK3P e d) = AsK3P (phantom_ref $ eRef e) d

  eTuple2 (AsK3P e1 d1, AsK3P e2 d2)
                     = AsK3P (eTuple2 (e1,e2))
                             (M.union d1 d2)
  eTuple3 (AsK3P e1 d1, AsK3P e2 d2, AsK3P e3 d3)
                     = AsK3P (eTuple3 (e1,e2,e3))
                             (M.union d1 $ M.union d2 d3)
  eTuple4 (AsK3P e1 d1, AsK3P e2 d2, AsK3P e3 d3, AsK3P e4 d4)
                     = AsK3P (eTuple4 (e1,e2,e3,e4))
                             (M.union (M.union d1 d2) (M.union d3 d4))
  eTuple5 (AsK3P e1 d1, AsK3P e2 d2, AsK3P e3 d3, AsK3P e4 d4, AsK3P e5 d5)
                     = AsK3P (eTuple5 (e1,e2,e3,e4,e5))
                             (M.union d1 $ M.union (M.union d2 d3)
                                                   (M.union d4 d5))

  eHL xs             = AsK3P (eHL $ hrlmap ask3p_exp xs)
                             (M.unions $ hrlproj ask3p_decls xs)

  eEmpty c             = AsK3P (phantom_coll $ eEmpty c) M.empty
  eSing  c (AsK3P e d) = AsK3P (phantom_coll $ eSing c e) d

  eLam p f           = 
    undefined
{-
    let earg x = phantom_pat_prf p x
        eres x = ask3p_exp (f (earg x))

    in AsK3P (phantom_lam p $ eLam p eres) undefined
-}
-}

------------------------------------------------------------------------}}}
-- Template Haskell splices                                             {{{


{-
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
-}
