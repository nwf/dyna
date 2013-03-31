---------------------------------------------------------------------------
-- | Things common to surface syntax representation of terms that are used
-- by several stages of the pipeline.

-- Header material                                                      {{{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Term.SurfaceSyntax where

import qualified Data.ByteString.UTF8       as BU
import qualified Data.Char                  as C
import qualified Data.Map                   as M
import           Dyna.Term.TTerm

------------------------------------------------------------------------}}}
-- Evaluation Disposition                                               {{{
-- Definition                                                           {{{

data SelfDispos = SDInherit
                | SDEval
                | SDQuote
 deriving (Eq,Show)

data ArgDispos = ADEval
               | ADQuote
 deriving (Eq,Show)

type DisposTab = M.Map (DFunct,Int) (SelfDispos,[ArgDispos])

------------------------------------------------------------------------}}}
-- Functions                                                            {{{

dtMerge = M.insert
{-# INLINE dtMerge #-}

fSelfEvalDispos :: DisposTab -> (DFunct, Int) -> SelfDispos
fSelfEvalDispos t fa = maybe def fst $ M.lookup fa t
 where
  def = let (name,_) = fa
        in maybe SDEval id $ fmap test $ BU.uncons name
  test (x,_) = if C.isAlphaNum x then SDInherit else SDEval

fArgEvalDispos :: DisposTab -> (DFunct, Int) -> [ArgDispos]
fArgEvalDispos t fa = maybe def snd $ M.lookup fa t
 where
  def = let (name,arity) = fa
        in take arity $ repeat
         $ maybe ADEval id $ fmap test $ BU.uncons name
  test (x,_) = if C.isAlphaNum x then ADQuote else ADEval

------------------------------------------------------------------------}}}
-- Defaults                                                             {{{

defDisposTab :: DisposTab
defDisposTab = M.fromList [
  -- math
    (("abs"  ,1),(SDEval,[ADEval]))
  , (("exp"  ,1),(SDEval,[ADEval]))
  , (("log"  ,1),(SDEval,[ADEval]))
  , (("mod"  ,2),(SDEval,[ADEval,ADEval]))
  -- logic
  , (("="    ,2),(SDEval,[ADQuote,ADQuote]))
  , (("and"  ,2),(SDEval,[ADEval, ADEval]))
  , (("or"   ,2),(SDEval,[ADEval, ADEval]))
  , (("not"  ,1),(SDEval,[ADEval]))
  -- structure
  , (("eval" ,1),(SDEval,[ADEval]))
  , (("pair" ,2),(SDQuote,[ADEval,ADEval]))
  , (("true" ,0),(SDQuote,[]))
  , (("false",0),(SDQuote,[]))
  ]

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
