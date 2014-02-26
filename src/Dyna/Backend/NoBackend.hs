---------------------------------------------------------------------------
-- | A backend that does no code generation.
--
-- It is anticipated that this will be useful for debugging the earlier
-- stages of the compiler.
--
-- XXX Add a self-test that all primOps modes are supported by other
-- backends.

-- Header material                                                      {{{

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Dyna.Backend.NoBackend (noBackend, primPossible) where

import           Control.Lens
import           Control.Monad
import qualified Data.Maybe                   as MA
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
          { be_aggregators    = Nothing
          , be_builtin        = primPossible
          , be_constants      = MA.isJust . primOps -- XXX
          , be_debug_dop_iter = \_ _ _ _ _ -> empty
          , be_driver         = driver
          }

driver _ _ _ _ _ _ fh = hPutStrLn fh "No backend selected; stopping."

------------------------------------------------------------------------}}}
-- Primitive operations                                                 {{{

data PrimOp = PO
            { po_qm :: QMode (NIX DFunct)
            , po_bs :: forall e . Doc e
            }

primOps :: DFunctAr -> Maybe [QMode (NIX DFunct)] -- XXX ,UMode
primOps = go
 where
  go ("-"    ,1) = Just $ miaod 1 Det ++ opinvd 1 Det
  go ("^"    ,2) = Just $ miaod 2 Det
  go ("|"    ,2) = Just $ miaod 2 Det
  go ("-"    ,2) = Just $ miaod 2 Det ++ opinvd 2 Det
  go ("/"    ,2) = Just $ miaod 2 Det
  go ("*"    ,2) = Just $ miaod 2 Det
  go ("**"   ,2) = Just $ miaod 2 Det
  go ("&"    ,2) = Just $ miaod 2 Det
  go ("%"    ,2) = Just $ miaod 2 Det
  go ("+"    ,2) = Just $ miaod 2 Det ++ opinvd 2 Det

  go ("mod"  ,1) = Just $ miaod 1 Det
  go ("abs"  ,1) = Just $ miaod 1 Det
  go ("log"  ,1) = Just $ miaod 1 Det
  go ("exp"  ,1) = Just $ miaod 1 Det

  go ("<="   ,2) = Just $ miaod 2 Det
  go ("<"    ,2) = Just $ miaod 2 Det
  go ("="    ,2) = Just $ miaod 2 Det
  go (">="   ,2) = Just $ miaod 2 Det
  go (">"    ,2) = Just $ miaod 2 Det
  go ("!="   ,2) = Just $ miaod 2 Det

  go ("and"  ,2) = Just $ miaod 2 Det
  go ("or"   ,2) = Just $ miaod 2 Det

  -- go ("null" ,0) = Just   miaod 0 Det

  go ("!"    ,1) = Just $ miaod 1 Det
  go ("not"  ,1) = Just $ miaod 1 Det

  go ("nil"  ,0) = Just   []
  go ("cons" ,2) = Just   []

  go _           = Nothing

primPossible :: (DFunct,[ModedVar],ModedVar) -> Either Bool (BackendAction ())
primPossible (f,mvis,mvo) = maybe (Left False) go $ primOps (f,length mvis)
 where
  go :: [QMode (NIX DFunct)] -> Either Bool (BackendAction ())
  go [] = Left True
  go (x:xs) = -- XT.traceShow ("PRIMPOSS",mvis,mvo,x) $
              if and (zipWithTails nSub p p qim pim)
               then Right $ BAct [OPIter mvo mvis f (x^.qmode_det) (Just ())] qom
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

-- | mode ins and out or ins and in, equally acceptable.
miaod ar d = map ($ d)
           [ QMode (replicate ar nnIn) nnOut
           , QMode (replicate ar nnIn) nnIn
           ]
-- | One-Place INVersion
opinvd ar d = map (\x -> QMode x nnIn d)
            $ map (\n ->    (replicate (n-1) nnIn)
                         ++ nnOut
                          : (replicate (ar-n) nnIn))
                  [1..ar]

------------------------------------------------------------------------}}}
