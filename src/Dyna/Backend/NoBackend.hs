---------------------------------------------------------------------------
-- | A backend that does no code generation.
--
-- It is anticipated that this will be useful for debugging the earlier
-- stages of the compiler.

-- Header material                                                      {{{

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Dyna.Backend.NoBackend (noBackend) where

import           Control.Lens
import           Control.Monad
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Dyna.Analysis.ANF
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.RuleMode (Actions,BackendAction(..),Cost)
import           Dyna.Backend.BackendDefn
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Dyna.XXX.PPrint
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free

import qualified Debug.Trace                as XT

------------------------------------------------------------------------}}}
-- Definition                                                           {{{

noBackend :: Backend
noBackend = Backend
          { be_builtin        = primPossible
          , be_constants      = M.keysSet primOps -- XXX
          , be_debug_dop_iter = \_ _ _ _ _ -> empty
          , be_driver         = driver
          }

driver am um {-_-} is fh = hPutStrLn fh "No backend selected; stopping."

------------------------------------------------------------------------}}}
-- Primitive operations                                                 {{{

data PrimOp = PO
            { po_qm :: QMode (NIX DFunct)
            , po_bs :: forall e . Doc e
            }

primOps :: M.Map DFunctAr [QMode (NIX DFunct)] -- XXX ,UMode
primOps = M.fromList
	[ let ar = 2 in ( ("+"     ,ar)      , [miaod ar Det     ]
                                           ++ opinvd ar Det )
    , let ar = 2 in ( ("-"     ,ar)      , [miaod ar Det     ]
                                           ++ opinvd ar Det )
    , let ar = 2 in ( ("*"     ,ar)      , [miaod ar Det     ]
                                           ++ opinvd ar DetSemi )
    , let ar = 2 in ( ("/"     ,ar)      , [miaod ar DetSemi ]
                                           ++ opinvd ar DetSemi )
    , let ar = 2 in ( ("^"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("&"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("|"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("%"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("**"    ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("<"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("<="    ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( (">"     ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( (">="    ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("="     ,ar)      , [miaod ar Det     ] )
	  -- Unary boolean negation
    , let ar = 1 in ( ("!"     ,ar)      , [miaod ar Det     ] )
	  -- Unary numeric negation
    , let ar = 1 in ( ("-"     ,ar)      , [miaod ar Det     ] )
    , let ar = 1 in ( ("mod"   ,ar)      , [miaod ar Det     ] )
    , let ar = 1 in ( ("abs"   ,ar)      , [miaod ar Det     ] )
    , let ar = 1 in ( ("log"   ,ar)      , [miaod ar Det     ] )
    , let ar = 1 in ( ("exp"   ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("and"   ,ar)      , [miaod ar Det     ] )
    , let ar = 2 in ( ("or"    ,ar)      , [miaod ar Det     ] )
    , let ar = 1 in ( ("not"   ,ar)      , [miaod ar Det     ] )
    ]

primPossible :: (DFunct,[ModedVar],ModedVar) -> Either Bool (BackendAction ())
primPossible (f,mvis,mvo) = maybe (Left False) go $ M.lookup (f,length mvis) primOps
 where
  go :: [QMode (NIX DFunct)] -> Either Bool (BackendAction ())
  go [] = Left True
  go (x:xs) = -- XT.traceShow ("PRIMPOSS",mvis,mvo,x) $
              if and (zipWithTails nSub p p qim pim)
               then Right $ BAct [OPIter mvo mvis f DetNon (Just ())] qom
               else go xs
    where
     mvs = mvo:mvis
     mds = x^.qmode_result : x^.qmode_args

     pim = fmap (^.mv_mi) mvs
     qim = fmap fst       mds

     qom = zipWithTails (,) p p 
                        (fmap (^.mv_var) mvs)
                        (fmap snd mds)

  p _ = dynacPanicStr "NoBackend.primPossible length mismatch"

------------------------------------------------------------------------}}}
-- Utility constructs                                                   {{{

nuniv, nfree :: NIX DFunct
nuniv = nHide (IUniv UShared)
nfree = nHide (IFree)

nnIn, nnOut :: (NIX DFunct, NIX DFunct)
nnIn  = (nuniv, nuniv)
nnOut = (nfree, nuniv)

-- | mode ins and out
miaod ar = QMode (replicate ar nnIn) nnOut

-- | One-Place INVersion
opinvd ar d = fmap (\x -> QMode x nnIn d) (go [] ar)
 where
  go _   0 = []
  go sfx n = (replicate n nnOut ++ sfx) : go (nnIn:sfx) (n-1)

------------------------------------------------------------------------}}}
