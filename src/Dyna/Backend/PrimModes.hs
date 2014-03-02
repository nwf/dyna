---------------------------------------------------------------------------
-- | The default query and update procedure bindings for primops
--
-- XXX This doesn't really belong in Backend but it's here to sit beside the
-- definition of the primops themselves.

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Backend.PrimModes (
    defProcs, defUpds
) where

import           Control.Applicative
import qualified Data.Map              as M
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.QueryProcTable
import           Dyna.Analysis.UpdateProcTable
import           Dyna.Backend.Primitives
import           Dyna.Term.SurfaceSyntax ( dynaConjOper
                                         , dynaRevConjOpers
                                         , dynaUnifOpers)
import           Dyna.Term.TTerm

------------------------------------------------------------------------}}}
-- Utility definitions                                                  {{{

-- XXX It's possible that these belong somewhere else, with better names?
-- We do seem to use them sprinkled about the source base.

ntru, nuniv, nfree :: NIX DFunct
ntru  = nHide (IBound UShared (M.fromList [("true" ,[])]) False)
nfls  = nHide (IBound UShared (M.fromList [("false",[])]) False)
nuniv = nHide (IUniv UShared)
nfree = nHide (IFree)

nnTru, nnFls, nnTro, nnIn, nnOut :: (NIX DFunct, NIX DFunct)
nnTru = (ntru , ntru )
nnFls = (nfls , nfls )
nnTro = (nfree, ntru )
nnIn  = (nuniv, nuniv)
nnOut = (nfree, nuniv)

mkFwdUnPrim :: DFunct -> (DVar -> DPrimOpF DVar) -> QueryProcEntry d
mkFwdUnPrim f p = QPE f nnOut [nnIn]       (\r [a]   -> OPPrim r (p a)   DetSemi)

mkFwdBiPrim :: DFunct -> (DVar -> DVar -> DPrimOpF DVar) -> QueryProcEntry d
mkFwdBiPrim f p = QPE f nnOut [nnIn, nnIn] (\r [a,b] -> OPPrim r (p a b) DetSemi)

------------------------------------------------------------------------}}}
-- Default procedures for constant primops                              {{{

constProcs :: [QueryProcEntry d]
constProcs =
  (uncurry mkFwdUnPrim <$>
        [ ("abs" , DPUnAbs )
         -- Skip DPUnEnum; handled below as part of "in/2"
        , ("^"   , DPUnExp )
        , ("exp" , DPUnExp )
        , ("!"   , DPUnLNeg)
        , ("log" , DPUnLog )
        , ("-"   , DPUnNNeg)
        , ("sqrt", DPUnSqrt)
        ])
  ++ (uncurry mkFwdBiPrim <$> 
        [("+"  , DPBiAdd  )
        ,("&"  , DPBiAnd  )
        ,(">=" , DPBiCmpGe)
        ,(">"  , DPBiCmpGt)
        ,("!=" , DPBiCmpNe)
        ,("<=" , DPBiCmpLe)
        ,("<"  , DPBiCmpLt)
        ,("/"  , DPBiDiv  )
        ,("**" , DPBiExp  )
        ,("exp", DPBiExp  )
        -- Skip DPBiIn; handled below as part of "in/2"
        ,("|"  , DPBiIor  )
        ,("log", DPBiIor  )
        ,("%"  , DPBiMod  )
        ,("mod", DPBiMod  )
        ,("*"  , DPBiMul  )
        ,("-"  , DPBiSub  )
        ,("^"  , DPBiXor  )
        ])
  ++ map (\f -> mkFwdBiPrim f DPBiCmpEq) dynaUnifOpers
  ++ [ -- Binary addition inverse mode
       QPE "+"  nnIn  [nnOut, nnIn ] (\r [a,b] -> OPPrim a (DPBiSub  r b) DetSemi)
     , QPE "+"  nnIn  [nnIn , nnOut] (\r [a,b] -> OPPrim b (DPBiSub  r a) DetSemi)
       -- Binary subtraction inverse mode
     , QPE "-"  nnIn  [nnOut, nnIn ] (\r [a,b] -> OPPrim a (DPBiAdd  r b) DetSemi)
     , QPE "-"  nnIn  [nnIn , nnOut] (\r [a,b] -> OPPrim b (DPBiSub  r a) DetSemi)
       -- Unary negation inverse mode
     , QPE "-"  nnIn  [nnOut       ] (\r [a]   -> OPPrim a (DPUnNNeg r  ) DetSemi)
       -- Membership testing
     , QPE "in" nnOut [nnIn , nnIn ] (\r [a,b] -> OPPrim r (DPBiIn   a b) DetSemi)
       -- Membership enumeration, when we know that membership must
       -- return true.
       --
       -- XXX This might have more refined detism if we have more
       -- refined inputs (is Det for singletons, DetFail for empty
       -- list)
     , QPE "in" nnTru [nnOut, nnIn ] (\_ [a,b] -> OPPrim a (DPUnEnum b  ) DetNon)
       -- Unary form of the above, with the true/0 implicit
     , QPE "in" nnOut [nnIn] (\r [a] -> OPPrim r (DPUnEnum a) DetNon)

       -- Additional modes for unification
       --  When we know the answer and both arguments
     , QPE "="  nnTru [nnIn , nnIn ] (\_ [a,b] -> OPCheq a b)
     , QPE "==" nnTru [nnIn , nnIn ] (\_ [a,b] -> OPCheq a b)
     , QPE "="  nnFls [nnIn , nnIn ] (\_ [a,b] -> OPCkne a b)
     , QPE "==" nnFls [nnIn , nnIn ] (\_ [a,b] -> OPCkne a b)
       --  When we know the answer and one argument
     , QPE "="  nnTru [nnOut, nnIn ] (\_ [a,b] -> OPAsnV a b)
     , QPE "==" nnTru [nnOut, nnIn ] (\_ [a,b] -> OPAsnV a b)
     , QPE "="  nnTru [nnIn , nnOut] (\_ [a,b] -> OPAsnV b a)
     , QPE "==" nnTru [nnIn , nnOut] (\_ [a,b] -> OPAsnV b a)

       -- Subgoal conjunction in its multiple guises
       --   Copy arg to result, with someone else having already
       --   proved the value of the left argument.
     , QPE dynaConjOper nnOut [nnTru, nnIn ] (\r [_,b] -> OPAsnV r b)
       --   Copy result to arg, same circumstance.
     , QPE dynaConjOper nnIn  [nnTru, nnOut] (\r [_,b] -> OPAsnV b r)
       --   Copy input to output, providing left a true value.
     , QPE dynaConjOper nnOut [nnTro, nnIn ] (\r [a,b] -> OPBloc [ OPWrap a [] "true"
                                                                 , OPAsnV r b
                                                                 ])

       --   Copy output to input, same.
     , QPE dynaConjOper nnIn  [nnTro, nnOut] (\r [a,b] -> OPBloc [ OPWrap a [] "true"
                                                                 , OPAsnV b r
                                                                 ])

     ]
     ++ concatMap (\f -> 
        [ QPE f nnOut [nnIn , nnTru] (\r [b,_] -> OPAsnV r b)
        , QPE f nnIn  [nnOut, nnTru] (\r [b,_] -> OPAsnV b r)
        , QPE f nnOut [nnIn , nnTro] (\r [b,a] -> OPBloc [ OPWrap a [] "true"
                                                         , OPAsnV r b
                                                         ])

        , QPE f nnIn  [nnOut, nnTro] (\r [b,a] -> OPBloc [ OPWrap a [] "true"
                                                         , OPAsnV b r
                                                         ])
        ]) dynaRevConjOpers

-- | Derive ``update forbidden'' entries for all constant primop bindings
constUpds :: [UpdateProcEntry d]
constUpds = map (\(QPE f _ as _) -> UPE (f,length as) UPSForbidden) constProcs

------------------------------------------------------------------------}}}
-- Non-const builtins                                                   {{{

nonconstProcs :: [QueryProcEntry d]
nonconstProcs = [ ]

-- | Derive ``update permitted'' entries for all nonconstant primop bindings
nonconstUpds :: [UpdateProcEntry d]
nonconstUpds = map (\(QPE f _ as _) -> UPE (f,length as) UPSAccepted) nonconstProcs

------------------------------------------------------------------------}}}
-- Exports                                                              {{{

defProcs :: [QueryProcEntry d]
defProcs = constProcs ++ nonconstProcs

defUpds :: [UpdateProcEntry d]
defUpds  = constUpds ++ nonconstUpds

------------------------------------------------------------------------}}}
