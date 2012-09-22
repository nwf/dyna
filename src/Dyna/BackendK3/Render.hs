---------------------------------------------------------------------------
-- Header material
------------------------------------------------------------------------{{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

    -- | Provides the "AsK3" type and instances for the K3 AST.
module Dyna.BackendK3.Render where

import           Text.PrettyPrint.Free

import           Dyna.BackendK3.AST


------------------------------------------------------------------------}}}
-- Type handling
------------------------------------------------------------------------{{{

    -- | Unlike AsK3 below, we don't need to thread a variable counter
    --   around since K3 doesn't have tyvars
newtype AsK3Ty e (a :: *) = AsK3Ty { unAsK3Ty :: Doc e }

instance K3Ty (AsK3Ty e) where
  tAnn (Ann anns) (AsK3Ty e) = AsK3Ty$ 
       parens e <> " @ "
    <> (encloseSep lbrace rbrace comma $ map text anns)

  tBool   = AsK3Ty$ "bool"
  tByte   = AsK3Ty$ "byte"
  tFloat  = AsK3Ty$ "float"
  tInt    = AsK3Ty$ "int"
  tString = AsK3Ty$ "string"
  tUnit   = AsK3Ty$ "unit"
  tUnk    = AsK3Ty$ "_"

  tPair (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ tupled [ ta, tb ]

  tMaybe (AsK3Ty ta) = AsK3Ty$ "Maybe " <> ta

  tColl CTSet  (AsK3Ty ta) = AsK3Ty$ braces   ta
  tColl CTBag  (AsK3Ty ta) = AsK3Ty$ encBag   ta
  tColl CTList (AsK3Ty ta) = AsK3Ty$ brackets ta

  tFun (AsK3Ty ta) (AsK3Ty tb) = AsK3Ty$ ta <> " -> " <> tb

------------------------------------------------------------------------}}}
-- Collection handling
------------------------------------------------------------------------{{{

class K3CFn (c :: CKind) where
  k3cfn_empty :: AsK3 e (CTE c a)
  k3cfn_sing  :: AsK3 e vma -> AsK3 e (CTE c vma)

instance K3CFn CSet where
  k3cfn_empty = AsK3$const$ "{ }"
  k3cfn_sing (AsK3 e) = AsK3$ braces . e

instance K3CFn CList where
  k3cfn_empty = AsK3$const$ "[ ]"
  k3cfn_sing (AsK3 e) = AsK3$ brackets . e

instance K3CFn CBag where
  k3cfn_empty = AsK3$const$ "{| |}"
  k3cfn_sing (AsK3 e) = AsK3$ encBag . e

------------------------------------------------------------------------}}}
-- Pattern handling
------------------------------------------------------------------------{{{

class (Pat w) => K3PFn w where
  k3pfn :: Int -> PatDa w -> (Int, Doc e, PatReprFn w (AsK3 e))

instance K3PFn (PKVar (a :: *)) where
  k3pfn n (PVar tr) = let sn = text $ "x" ++ show n in
                      (n+1
                      ,sn <> colon <> unAsK3Ty (unETR tr)
                      ,AsK3$const$ sn)

instance (K3PFn w) => K3PFn (PKJust w) where
  k3pfn n (PJust w) = let (n', b, r) = k3pfn n w
                      in (n', "Just " <> parens b, r)

instance (K3PFn wa, K3PFn wb) => K3PFn (PKPair wa wb) where
  k3pfn n (PPair wa wb) =
    let (n', ba, ra) = k3pfn n wa
        (n'', bb, rb) = k3pfn n' wb
    in (n'', tupled [ ba, bb ], (ra,rb))

------------------------------------------------------------------------}}}
-- Slice handling
------------------------------------------------------------------------{{{

class (Slice w) => K3SFn w where
  k3sfn :: SliceDa w -> Doc e

instance K3SFn (SKVar (a :: *)) where
  k3sfn (SVar (Var v) _) = text v

instance (K3SFn s) => K3SFn (SKJust s) where
  k3sfn (SJust s) = "Just" <> parens (k3sfn s)

instance (K3SFn sa, K3SFn sb) => K3SFn (SKPair sa sb) where
  k3sfn (SPair sa sb) = tupled [ k3sfn sa, k3sfn sb ]

------------------------------------------------------------------------}}}
-- Expression handling
------------------------------------------------------------------------{{{

newtype AsK3 e (a :: *) = AsK3 { unAsK3 :: Int -> Doc e }

instance K3 (AsK3 e) where
  type K3AST_Coll_C (AsK3 e) c = K3CFn c
  type K3AST_Pat_C (AsK3 e) p = K3PFn p
  type K3AST_Slice_C (AsK3 e) s = K3SFn s

  cAnn (Ann anns) (AsK3 e) = AsK3$ \n ->
       parens (e n) <> " @ "
    <> (encloseSep lbrace rbrace comma $ map text anns)

  cComment str (AsK3 a) = AsK3$ \n -> "\n// " <> text str <> "\n" <> a n

  cBool   n     = AsK3$const$ text$ show n
  cByte   n     = AsK3$const$ text$ show n
  cFloat  n     = AsK3$const$ text$ show n
  cInt    n     = AsK3$const$ text$ show n
  cString n     = AsK3$const$ text$ show n
  cNothing      = AsK3$const$ "nothing"
  cUnit         = AsK3$const$ "unit"
  cUnk          = AsK3$const$ "_"

  eVar (Var v) = AsK3$const$text v


  ePair (AsK3 a) (AsK3 b) = AsK3$ \n -> tupled [a n, b n]
  eJust (AsK3 a)          = builtin "Just " [ a ]

  eEmpty = k3cfn_empty
  eSing  = k3cfn_sing
  eComb (AsK3 a) (AsK3 b) = AsK3$ \n -> parens (a n) <> " ++ " <> parens (b n)
  eRange (AsK3 f) (AsK3 l) (AsK3 s) = builtin "range" [ f, l, s ]
  
  eAdd (AsK3 a) (AsK3 b) = AsK3$ \n -> a n <> "+" <> b n
  eMul (AsK3 a) (AsK3 b) = AsK3$ \n -> a n <> "*" <> b n
  eNeg (AsK3 b) = AsK3$ \n -> "-" <> parens (b n)

  eEq  = binop "=="
  eLt  = binop "<"
  eLeq = binop "<="
  eNeq = binop "!="

  eLam w f = AsK3$ \n -> let (n', pat, arg) = k3pfn n w
                         in "\\" <> pat <> " -> " <> (unAsK3 (f arg) n')

  eApp (AsK3 f) (AsK3 x) = AsK3$ \n ->
    parens (parens (f n) <> space <> parens (x n))

  eBlock ss (AsK3 r) = AsK3$ \n -> 
    "do" <> (semiBraces (map ($ n) ((map unAsK3 ss) ++ [r])))

  eIter (AsK3 f) (AsK3 c) = builtin "iterate" [ f, c ]

  eITE (AsK3 b) (AsK3 t) (AsK3 e) = AsK3$ \n ->    "if "     <> parens (b n)
                                                <> " then "  <> parens (t n)
                                                <> " else "  <> parens (e n)

  eMap     (AsK3 f) (AsK3 c)                   = builtin "map"       [ f, c    ]
  eFiltMap (AsK3 f) (AsK3 m) (AsK3 c)          = builtin "filtermap" [ f, m, c ]
  eFlatten (AsK3 c)                            = builtin "flatten"   [ c ]
  eFold    (AsK3 f) (AsK3 z) (AsK3 c)          = builtin "fold"      [ f, z, c ]
  eGBA     (AsK3 p) (AsK3 f) (AsK3 z) (AsK3 c) = builtin "groupby"   [ p, f, z, c ]
  eSort    (AsK3 c) (AsK3 f)                   = builtin "sort"      [ c, f ]
  ePeek    (AsK3 c)                            = builtin "peek"      [ c ]

  eSlice w (AsK3 c) = AsK3$ \n -> c n <> brackets (k3sfn w)

  eInsert (AsK3 c) (AsK3 e)          = builtin "insert" [ c, e ]
  eDelete (AsK3 c) (AsK3 e)          = builtin "delete" [ c, e ]
  eUpdate (AsK3 c) (AsK3 o) (AsK3 n) = builtin "update" [ c, o, n ]

------------------------------------------------------------------------}}}
-- Miscellany
------------------------------------------------------------------------{{{

encBag :: Doc e -> Doc e
encBag = enclose "{|" "|}"

    -- Overly polymorphic; use only when correct
binop :: Doc e -> AsK3 e a -> AsK3 e a -> AsK3 e b
binop o (AsK3 a) (AsK3 b) = AsK3$ \n -> parens (a n) <> o <> parens (b n)

    -- Overly polymorphic; use only when correct
builtin :: Doc e -> [ Int -> Doc e ] -> AsK3 e b
builtin fn as = AsK3$ \n -> fn <> tupled (map ($ n) as)

instance Show (AsK3 e a) where
  show (AsK3 f) = show $ f 0

sh :: AsK3 e a -> String
sh = show

instance Show (AsK3Ty e a) where
  show (AsK3Ty f) = show f

sht :: AsK3Ty e a -> String
sht = show

shd :: Decl (AsK3Ty e) (AsK3 e) t -> Doc e
shd (Decl (Var name) tipe body) =
     "declare "
  <> text name
  <> space <> colon <> space
  <> unAsK3Ty tipe
  <> case body of
       Nothing -> empty
       Just b  -> space <> equals <> space <> unAsK3 b 0
  <> semi

------------------------------------------------------------------------}}}
-- fin
---------------------------------------------------------------------------
