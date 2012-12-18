---------------------------------------------------------------------------
-- | Collect the declarations used inside a K3 expression.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Dyna.Backend.K3.CollectDecls where

import qualified Data.Map as M

import Dyna.Backend.K3.AST
import Dyna.XXX.HList

data ExDecl = forall a s . Ex (Decl s a)

newtype C (a :: *) = C { unC :: M.Map VarIx ExDecl }

pC :: C a -> C b
pC (C x) = C x

eC :: C a
eC = C M.empty

uC :: C a -> C b -> C c
uC (C a) (C b) = C $ M.union a b

mkpc :: PDat UnivTyRepr w -> (PatReprFn C w)
mkpc (PVar _)          = eC
mkpc PUnk              = ()
mkpc (PHL HRN)         = HN
mkpc (PHL (w :++ ws))  = mkpc w :+ mkpc (PHL ws)
mkpc (PJust x)         = mkpc x
mkpc (PRef x)          = mkpc x
mkpc (PT2 (a,b))       = (mkpc a, mkpc b)
mkpc (PT3 (a,b,c))     = (mkpc a, mkpc b, mkpc c)
mkpc (PT4 (a,b,c,d))   = (mkpc a, mkpc b, mkpc c, mkpc d)
mkpc (PT5 (a,b,c,d,e)) = (mkpc a, mkpc b, mkpc c, mkpc d, mkpc e)
-- XXX k3ref
-- mkpc (PRef w)         = mkpc w

cslice :: PDat C w -> C (PatTy C w)
cslice (PVar x)          = x
cslice PUnk              = eC
cslice (PJust x)         = pC $ cslice x
cslice (PRef x)          = pC $ cslice x
cslice (PHL xs)          = C $ M.unions $ hrlproj (unC . cslice) xs
cslice (PT2 (a,b))       = uC (cslice a) (cslice b)
cslice (PT3 (a,b,c))     = C $ M.unions [ unC $ cslice a
                                        , unC $ cslice b
                                        , unC $ cslice c
                                        ]
cslice (PT4 (a,b,c,d))   = C $ M.unions [ unC $ cslice a
                                        , unC $ cslice b
                                        , unC $ cslice c
                                        , unC $ cslice d
                                        ]
cslice (PT5 (a,b,c,d,e)) = C $ M.unions [ unC $ cslice a
                                        , unC $ cslice b
                                        , unC $ cslice c
                                        , unC $ cslice d
                                        , unC $ cslice e
                                        ]

instance K3 C where
  declVar d@(Decl v b) = C $ M.union (cdk b) (M.singleton v (Ex d))
  unsafeVar _ _        = eC
  
  cComment _ x         = x
  cAnn x _             = x

  cAddress _           = eC
  cBool _              = eC
  cByte _              = eC
  cFloat _             = eC
  cInt _               = eC
  cNothing             = eC
  cString _            = eC
  cUnit                = eC

  eJust c              = pC c
  -- XXX k3ref
  -- eRef c               = pC c

  eTuple2 (a,b)        = C $ M.unions [unC a, unC b]
  eTuple3 (a,b,c)      = C $ M.unions [unC a, unC b, unC c]
  eTuple4 (a,b,c,d)    = C $ M.unions [unC a, unC b, unC c, unC d]
  eTuple5 (a,b,c,d,e)  = C $ M.unions [unC a, unC b, unC c, unC d, unC e]

  eHL HRN              = eC
  eHL (a:++b)          = uC a (eHL b)

  eEmpty _             = eC
  eSing _ c            = pC c
  eCombine a b         = uC a b
  eRange a b c         = C $ M.unions [unC a, unC b, unC c]

  eAdd a b             = uC a b
  eMul a b             = uC a b
  eNeg a               = pC a

  eEq  a b             = uC a b
  eLt  a b             = uC a b
  eLeq a b             = uC a b
  eNeq a b             = uC a b

  eLam p fn            = let cs = unC $ fn (mkpc p) in C cs
  eApp f x             = uC f x

  eBlock cs c          = C $ M.unions (unC c : map unC cs)
  eIter f c            = uC f c
  eITE i t e           = C $ M.unions [unC i, unC t, unC e]

  eMap       f c       = uC f c
  eFiltMap p f c       = C $ M.unions [unC p, unC f, unC c]

  eFlatten c           = pC c

  eFold s z c          = C $ M.unions [unC s, unC z, unC c]

  eGBA p f z i         = C $ M.unions [unC p, unC f, unC z, unC i]

  eSort i c            = uC i c

  ePeek c              = pC c
  eSlice s c           = uC (cslice s) c

  eInsert a b          = uC a b
  eDelete a b          = uC a b
  eUpdate a b c        = C $ M.unions [unC a, unC b, unC c]

  eAssign r v          = C $ M.union (unC r) (unC v)

  eSend a t m          = C $ M.unions [unC a, unC t, unC m]

cdk :: DBody t -> M.Map VarIx ExDecl
cdk (DColl _)       = M.empty
cdk (DRef _ i)      = unC i
cdk (DFunc _ _ p f) = unC (f $ mkpc p)
cdk (DTrig p f)     = unC (f $ mkpc p)
