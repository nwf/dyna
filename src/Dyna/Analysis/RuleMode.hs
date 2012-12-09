---------------------------------------------------------------------------
-- | Mode analysis of a rule
--
-- XXX Gotta start somewhere.

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.RuleMode where

import           Control.Monad
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.Char
-- import           Data.Either
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Ord                   as O
import qualified Data.Set                   as S
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as DP
import           Dyna.XXX.PPrint
import           Dyna.XXX.TrifectaTest
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

filterNTs = MA.mapMaybe isNTVar
 where
  isNTVar (NTVar x) = Just x
  isNTVar _         = Nothing

------------------------------------------------------------------------}}}
-- Modes                                                                {{{

data Mode = MFree | MBound deriving (Eq,Ord,Show)

-- | What things have thus far been bound under the plan?
type BindChart = S.Set DVar

varMode :: BindChart -> NTV -> Mode
varMode c (NTVar v) = if v `S.member` c then MBound else MFree
varMode c (NTString _) = MBound
varMode c (NTNumeric _) = MBound

type ModedNT = NT (Mode,DVar)

modeOfMNT :: ModedNT -> Mode
modeOfMNT (NTNumeric _) = MBound
modeOfMNT (NTString _)  = MBound
modeOfMNT (NTVar (m,_)) = m

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

type Crux n = (DFunct,[n],n)

cruxMode :: Crux NTV -> BindChart -> Crux ModedNT
cruxMode (f,is,o) c = (f, map (mode c) is, mode c o)
 where
  mode c x@(NTVar v)   = NTVar (varMode c x, v)
  mode _ (NTString s)  = NTString s
  mode _ (NTNumeric x) = NTNumeric x

------------------------------------------------------------------------}}}
-- Steps, Actions, and Plans                                            {{{

data Det = Det          -- ^ Exactly one answer
         | DetSemi      -- ^ At most one answer
         | DetNon       -- ^ Unknown number of answers
 deriving (Eq,Ord,Show)

type Step = (DFunct, [ModedNT], ModedNT, Det)

-- | A 'Step' that indicates a need to check two variables' values being
-- equal.
checkStep :: NTV -> NTV -> Step
checkStep ex va = ("=", [mode va], mode ex, DetSemi)
 where
  mode x@(NTVar v)   = NTVar (MBound, v)
  mode (NTString s)  = NTString s
  mode (NTNumeric x) = NTNumeric x

type Action = [Step]

type Score = Double

data PartialPlan = PP { pp_cruxes :: S.Set (Crux NTV)
                      , pp_binds  :: BindChart
                      , pp_score  :: Score
                      , pp_plan   :: Action
                      }

stepPartialPlan :: (Crux ModedNT -> [Action])
                -> (PartialPlan -> Action -> Score)
                -> PartialPlan
                -> Either (Score, Action) [PartialPlan]
stepPartialPlan steps score p =
  if S.null (pp_cruxes p)
   then Left $ (pp_score p, pp_plan p)
   else Right $
    let rc = pp_cruxes p
    in  S.fold (\(crux@(_,vis,vo)) ps -> (
                let bc = pp_binds p
                    pl = pp_plan  p
                    plans = steps (cruxMode crux bc)
                    bc' = bc `S.union` (S.fromList $ filterNTs (vo:vis))
                    rc' = S.delete crux rc
                in map (\act -> PP rc' bc' (score p act) (pl ++ act))
                       plans
                 ) ++ ps
               ) [] rc

stepAgenda st sc = go
 where
  go []     = []
  go (p:ps) = case stepPartialPlan st sc p of
                    Left df -> df : (go ps)
                    Right ps' -> go (ps'++ps)

eval_cruxes = M.foldWithKey (\o i -> (crux o i :)) [] . as_evals
 where
  crux :: DVar -> EVF -> Crux NTV
  crux o (Left v) = ("*",[NTVar v],NTVar o)
  crux o (Right (TFunctor n as)) = (n,as,NTVar o)

unif_cruxes = M.foldWithKey (\o i -> (crux o i :)) [] . as_unifs
 where
  crux :: DVar -> FDT -> Crux NTV
  crux o t@(TString s) = ("=",[NTString s], NTVar o)
  crux o t@(TNumeric n) = ("=",[NTNumeric n], NTVar o)
  crux o (TFunctor x as) = (B.append "&" x, as, NTVar o)

-- | Given a normalized form and an initial crux, saturate the graph and
--   get a plan for doing so.
plan :: (Crux ModedNT -> [Action])
     -> (PartialPlan -> Action -> Score)
     -> (FDR, ANFState)
     -> Crux NTV
     -> (Score, Action)
plan st sc (fr, anfs) cr@(c,ci,co) = 
  let cruxes =    eval_cruxes anfs
               ++ unif_cruxes anfs
      initPlan = PP { pp_cruxes = S.delete cr (S.fromList cruxes)
                    , pp_binds  = S.fromList $ filterNTs (co:ci)
                    , pp_score  = 0
                    , pp_plan   = []
                    }
  in L.minimumBy (O.comparing fst) $ stepAgenda st sc [initPlan]

------------------------------------------------------------------------}}}
-- Possible steps                                                       {{{

-- XXX
isMath f = f `elem` ["^", "+", "-", "*", "/"]

-- XXX This function really ought to be generated from some declarations in
-- the source program, rather than hard-coded.
possible :: Crux ModedNT -> [Action]
possible (f,is,o) = case () of
    -- Check
  _ | f == "=" && length is == 1 -> [[("=",is,o,DetSemi)]]

    -- Unification
  _ | B.take 1 f == "&" -> 
      let funct = B.drop 1 f in
      case modeOfMNT o of
        -- If the output is free, the only supported case is when all
        -- inputs are known.
        MFree  -> if all isBound is
                   then [[("&",is,o,Det)]]
                   else []
        -- On the other hand, if the output is known, then any subset
        -- of the inputs may be known and will be checked.
        --
        -- XXX Does not understand nonlinear patterns D:
        MBound -> let chkf = "_chk_f"
                      mkChks n x | isBound x
                                 = let chk = "_chk_" -- XXX
                                   in ( NTVar (MFree,chk)
                                      , Just (chk,x))
                      mkChks _ x = (x, Nothing)

                      (is',mcis) = unzip $ zipWith mkChks [0..] is
                      cis        = MA.catMaybes mcis
                  in [  ("&",is',o,Det)
                      : map (\(c,x) -> ("=",[NTVar (MBound,c)],x,DetSemi))
                            cis
                     ]

    -- Backward-chainable mathematics (this is such a hack XXX)
  _ | isMath f ->
      if not $ all isBound is
       then case inv f is o of
              Nothing -> []
              Just (f',is',o') -> [[(f',is',o',Det)]]
       else case modeOfMNT o of
              MFree ->  [[(f,is,o,Det)]]
              MBound -> let cv = "_chk"
                        in [[(f,is,NTVar (MFree,cv),DetSemi)
                            ,("=",[NTVar (MBound,cv)],o,DetSemi)
                            ]]
  _ | otherwise ->
      if all isBound (o:is)
       then let cv = "_chk"
            in [[(f,is,NTVar (MFree,cv),DetSemi)
                ,("=",[NTVar (MBound,cv)],o,DetSemi)
                ]]
       else [[(f,is,o,DetNon)]]

 where
  isBound = (== MBound) . modeOfMNT
  isFree  = (== MFree) . modeOfMNT

  inv "+" is o | length is == 2 && isBound o
               = case L.partition isFree is of
                   ([fi],bis) -> Just ("-",o:bis,fi)

  inv "-" [x,y] o | isBound x && isBound o && isFree y
                  = Just ("-",[x,o],y)

  inv "-" [x,y] o | isBound y && isBound o && isFree x
                  = Just ("+",[o,y],x)

  inv _   _  _  = Nothing

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

testPlanRule x =
 let (fr,anfs) = runNormalize $ normRule (unsafeParse DP.drule x)
     updatePlans = map (\c -> (c, plan possible (\_ _ -> 0) (fr,anfs) c))
       $ filter (\(f,_,_) -> not $ isMath f)
       $ eval_cruxes anfs
  in updatePlans

main :: IO ()
main = mapM_ (\(c,(s,p)) -> do
                putStrLn $ show c
                putStrLn $ "SCORE: " ++ show s
                forM_ p (putStrLn . show))
       $ testPlanRule
       -- $ "fib(X) :- fib(X-1) + fib(X-2)"
       $ "path(pair(Y,Z),V) min= path(pair(X,Y),U) + cost(X,Y,Z,U,V)."
       -- $ "goal += f(&pair(Y,Y))." -- 

------------------------------------------------------------------------}}}
