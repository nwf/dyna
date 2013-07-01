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
import           Data.String
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Text.Parser.Expression (Assoc(..))

------------------------------------------------------------------------}}}
-- Keywords                                                             {{{

-- These are defined here rather than being implicit in Dyna.Analysis.ANF.
--
-- If we ever revisit the structure of rules, cross-ref XREF:ANFRESERVED and
-- maybe move all of this into the parser proper.

dynaEvalOper :: (IsString s) => s
dynaEvalOper  = "*"

dynaQuoteOper :: (IsString s) => s
dynaQuoteOper = "&"

dynaEvalAssignOper :: (IsString s) => s
dynaEvalAssignOper = "is"

dynaConjOper :: (IsString s) => s
dynaConjOper = ","

dynaRevConjOpers :: (IsString s) => [s]
dynaRevConjOpers = ["whenever","for"]

dynaUnitTerm :: (IsString s) => s
dynaUnitTerm = "true"

dynaUnifOpers :: (IsString s) => [s]
dynaUnifOpers = [ "=", "==" ]

------------------------------------------------------------------------}}}
-- Operators                                                            {{{

data Fixity = PFIn Assoc | PFPre | PFPost
 deriving (Eq,Show)

-- | For each possible operator symbol, specify its precedence and fixity.
--
-- For the precedence, a higher number means tighter binding.
type OperSpec = M.Map String [(Int, Fixity)]

-- | The basic expression table for limited expressions.
--
-- Notably, this excludes @,@ (which is important
-- syntactically), @for@, @whenever@, and @is@ (which are
-- nonsensical in local context)
--
-- The precedence and fixity here are mostly as per Haskell 98.
defOperSpec :: OperSpec
defOperSpec = foldr (\(k,v) -> mapInOrCons k v) def more
 where
  def = M.fromList
    [ ("-"  ,[(6,PFIn AssocLeft ), (9, PFPre)])
    , ("^"  ,[(8,PFIn AssocLeft )            ])
    , ("|"  ,[(2,PFIn AssocRight)            ])
    , ("/"  ,[(7,PFIn AssocLeft )            ])
    , ("*"  ,[(7,PFIn AssocLeft )            ])
    , ("**" ,[(8,PFIn AssocRight)            ])
    , ("&"  ,[(3,PFIn AssocRight)            ])
    , ("%"  ,[(7,PFIn AssocLeft )            ])
    , ("+"  ,[(6,PFIn AssocLeft )            ])

    , ("in" ,[(4,PFIn AssocNone )            ])

    , ("<=" ,[(4,PFIn AssocNone )            ])
    , ("<"  ,[(4,PFIn AssocNone )            ])
    , ("="  ,[(4,PFIn AssocNone )            ])
    , ("==" ,[(4,PFIn AssocNone )            ])
    , (">=" ,[(4,PFIn AssocNone )            ])
    , (">"  ,[(4,PFIn AssocNone )            ])
    , ("!=" ,[(4,PFIn AssocNone )            ])

    , ("!"  ,[(9,PFPre)                      ])

    , ("new",[(0,PFPre)                      ])
    ]

  more = [(dynaQuoteOper, (9,PFPre))
         ,(dynaEvalOper, (9,PFPre))
         ]

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

type DisposTabOver = M.Map DFunctAr (SelfDispos,[ArgDispos])

data DisposTab = DisposTab
               { dt_selfEvalDispos :: DFunctAr -> SelfDispos
               , dt_argEvalDispos  :: DFunctAr -> [ArgDispos]
               }

------------------------------------------------------------------------}}}
-- Functions                                                            {{{

dtoMerge :: DFunctAr
         -> (SelfDispos,[ArgDispos])
         -> DisposTabOver
         -> DisposTabOver
dtoMerge = M.insert
{-# INLINE dtoMerge #-}

------------------------------------------------------------------------}}}
-- Defaults                                                             {{{

-- | Make the default surface syntax look like a kind of prolog with funny
-- operators.  In particular all initial-alphanumeric functors inherit and
-- prefer to /quote/ their arguments, while initial-symbolic functors
-- request their own evaluation and the evaluation of their arguments.
--
-- Notably, TimV seems to prefer this syntax.
disposTab_prologish :: DisposTabOver -> DisposTab
disposTab_prologish t = DisposTab s a
 where
  s :: (DFunct, Int) -> SelfDispos
  s fa = maybe (maybe def fst $ M.lookup fa dt) fst $ M.lookup fa t
   where
    def = let (name,_) = fa
          in maybe SDEval id $ fmap test $ BU.uncons name
    test (x,_) = if C.isAlphaNum x then SDInherit else SDEval

  a :: (DFunct, Int) -> [ArgDispos]
  a fa = maybe (maybe def snd $ M.lookup fa dt) snd $ M.lookup fa t
   where
    def = let (name,arity) = fa
          in take arity $ repeat
           $ maybe ADEval id $ fmap test $ BU.uncons name
    test (x,_) = if C.isAlphaNum x then ADQuote else ADEval

  -- A built-in set of defaults, used if we miss the user-provided table
  -- but before we fall-back to the default rules.
  dt = M.fromList [
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
       -- lists
       , (("nil",  0),(SDQuote,[]))
       , (("cons", 2),(SDQuote,[ADEval,ADEval]))
       ]

-- | Make the default surface syntax more functional.  Here, all functors
-- inherit their self disposition from context and always prefer to evaluate
-- their arguments.
disposTab_dyna :: DisposTabOver -> DisposTab
disposTab_dyna t = DisposTab s a
 where
  s :: (DFunct, Int) -> SelfDispos
  s fa = maybe (maybe SDInherit fst $ M.lookup fa dt) fst $ M.lookup fa t

  a :: (DFunct, Int) -> [ArgDispos]
  a fa@(_,arity) = maybe (maybe def snd $ M.lookup fa dt) snd $ M.lookup fa t
   where
    def = take arity $ repeat ADEval

  -- There are, however, even in this case a few terms we would prefer to
  -- behave structurally by default.
  dt = M.fromList [
         (("="    ,2),(SDEval ,[ADQuote,ADQuote]))
       , (("pair" ,2),(SDQuote,[ADEval ,ADEval ]))
       -- booleans
       , (("true" ,0),(SDQuote,[]))
       , (("false",0),(SDQuote,[]))
       -- lists
       , (("nil",  0),(SDQuote,[]))
       , (("cons", 2),(SDQuote,[ADEval,ADEval]))
       ]

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
