---------------------------------------------------------------------------
-- | Mode analysis of a rule
--
-- Takes input from "Dyna.Analysis.ANF"
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

data Mode = MBound | MFree deriving (Eq,Ord,Show)

-- | What things have thus far been bound under the plan?
type BindChart = S.Set DVar

varMode :: BindChart -> NTV -> Mode
varMode c (NTVar v) = if v `S.member` c then MBound else MFree
varMode c (NTString _) = MBound
varMode c (NTNumeric _) = MBound

type ModedVar = (Mode,DVar)

data ModedNT = MF DVar
             | MB NTV
 deriving (Eq,Ord,Show)

modeOfMNT :: ModedNT -> Mode
modeOfMNT (MF _) = MFree 
modeOfMNT (MB _) = MBound

ntvOfMNT :: ModedNT -> NTV
ntvOfMNT (MB x) = x
ntvOfMNT (MF v) = NTVar v

isBound, isFree :: ModedNT -> Bool
isBound = (== MBound) . modeOfMNT
isFree  = (== MFree) . modeOfMNT

data Det = Det          -- ^ Exactly one answer
         | DetSemi      -- ^ At most one answer
         | DetNon       -- ^ Unknown number of answers
 deriving (Eq,Ord,Show)

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data CFunct = CFCall DFunct
            | CFUnif DFunct
            | CFAssign
            | CFEval
 deriving (Eq,Ord,Show)

type Crux n = (CFunct,[n],n)

cruxMode :: Crux NTV -> BindChart -> Crux ModedNT
cruxMode (f,is,o) c = (f, map (mode c) is, mode c o)
 where
  mode c x@(NTVar v)   = case varMode c x of
                           MBound -> MB x
                           MFree  -> MF v
  mode _ (NTString s)  = MB (NTString s)
  mode _ (NTNumeric x) = MB (NTNumeric x)

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.

--              Opcode          Out     In
data DOpAMine = OPAssign        DVar    NTV                   --  -+
              | OPCheck         DVar    NTV                   --  ++

              | OPCheckFunctor          DVar      DFunct Int  --   +
              | OPGetArgs       [DVar]  DVar                  --  -+
              | OPBuild         DVar    [NTV]     DFunct      --  -+

              | OPCall          DVar    [NTV]     DFunct      --  -+
              | OPIter          ModedNT [ModedNT] DFunct      --  ??
              | OPIndirEval     DVar    DVar                  --  -+
 deriving (Eq,Ord,Show)

detOfDop :: DOpAMine -> Det
detOfDop x = case x of
               OPAssign _ _         -> Det
               OPCheck _ _          -> DetSemi
               OPCheckFunctor _ _ _ -> DetSemi
               OPGetArgs _ _        -> Det
               OPBuild _ _ _        -> Det
               OPIndirEval _ _      -> DetSemi
               OPCall _ _ _         -> Det
               OPIter o is _        -> -- XXX
                    case (modeOfMNT o, foldr min MBound (map modeOfMNT is)) of
                      (MFree, MBound) -> DetSemi
                      _               -> DetNon

------------------------------------------------------------------------}}}
-- Actions                                                              {{{

type Action = [DOpAMine]

-- XXX
isMath f = f `elem` ["^", "+", "-", "*", "/"]

-- XXX This function really ought to be generated from some declarations in
-- the source program, rather than hard-coded.
possible :: Crux ModedNT -> [Action]
possible (f,is,o) = case f of
    -- XXX Indirect evaluation is not yet supported
  CFEval -> []

    -- Assign or check
  CFAssign -> case is of
                    [i] -> case (i, o) of
                             (MF _, MF _)   -> []
                             (MB i', MB o') -> let chk = "_chk" in
                                               [[ OPAssign chk i'
                                                , OPCheck  chk o']]
                             (MF o', MB i') -> [[OPAssign o' i']]
                             (MB i', MF o') -> [[OPAssign o' i']]
                    _   -> []

    -- Unification
  CFUnif funct -> 
      case o of
        -- If the output is free, the only supported case is when all
        -- inputs are known.
        MF o'  -> if all isBound is
                   then let is' = map ntvOfMNT is
                        in [[OPBuild o' is' funct]]
                   else []
        -- On the other hand, if the output is known, then any subset
        -- of the inputs may be known and will be checked.
        --
        -- XXX Does not understand nonlinear patterns D:
        MB (NTVar o') -> [   (OPCheckFunctor o' funct $ length is)
                           : (OPGetArgs is' o')
                           : map (\(c,x) -> (OPCheck c x)) cis
                         ]
         where
          mkChks n (MF i) = (i, Nothing)
          mkChks n (MB v) = let chk = BC.pack $ "_chk_" ++ (show n)
                            in (chk, Just (chk, v))

          (is',mcis) = unzip $ zipWith mkChks [0..] is
          cis        = MA.catMaybes mcis
          
    -- Backward-chainable mathematics (this is such a hack XXX)
  CFCall f | isMath f ->
      if not $ all isBound is
       then case inv f is o of
              Nothing -> []
              Just (f',is',o') -> [[OPCall o' is' f']]
       else let is' = map ntvOfMNT is in
            case o of
              MF o' ->  [[OPCall o' is' f]]
              MB o' -> let cv = "_chk"
                       in [[OPCall  cv is' f
                           ,OPCheck cv o'
                           ]]

  CFCall f | otherwise -> [[OPIter o is f ]]

 where
  inv "+" is o | length is == 2 && isBound o
               = case L.partition isFree is of
                   ([MF fi],bis) -> Just ("-",map ntvOfMNT $ o:bis,fi)
                   _ -> Nothing

  inv "-" [(MB x),(MF y)] (MB o)
                  = Just ("-",[x,o],y)

  inv "-" [(MF x),(MB y)] (MB o)
                  = Just ("+",[o,y],x)
  inv _   _  _  = Nothing


------------------------------------------------------------------------}}}
-- Plans                                                                {{{

type Cost = Double

data PartialPlan = PP { pp_cruxes :: S.Set (Crux NTV)
                      , pp_binds  :: BindChart
                      , pp_score  :: Cost
                      , pp_plan   :: Action
                      }

stepPartialPlan :: (Crux ModedNT -> [Action])
                -> (PartialPlan -> Action -> Cost)
                -> PartialPlan
                -> Either (Cost, Action) [PartialPlan]
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

------------------------------------------------------------------------}}}
-- Costing Plans                                                        {{{

simpleCost :: PartialPlan -> Action -> Cost
simpleCost (PP { pp_score = osc }) act =
    osc + sum (map stepCost act)
 where
  stepCost :: DOpAMine -> Double
  stepCost x = case x of
    OPAssign _ _         -> 0
    OPCheck _ _          -> 1
    OPCheckFunctor _ _ _ -> 0
    OPGetArgs _ _        -> 0
    OPBuild _ _ _        -> 0
    OPCall _ _ _         -> 0
    OPIter o is _        -> fromIntegral $ length $ filter isFree (o:is)
    OPIndirEval _ _      -> 10

------------------------------------------------------------------------}}}
-- ANF to Cruxes                                                        {{{

eval_cruxes = M.foldrWithKey (\o i -> (crux o i :)) [] . as_evals
 where
  crux :: DVar -> EVF -> Crux NTV
  crux o (Left v) = (CFEval,[NTVar v],NTVar o)
  crux o (Right (TFunctor n as)) = (CFCall n,as,NTVar o)

unif_cruxes = M.foldrWithKey (\o i -> (crux o i :)) [] . as_unifs
 where
  crux :: DVar -> FDT -> Crux NTV
  crux o t@(TString s) = (CFAssign,[NTString s], NTVar o)
  crux o t@(TNumeric n) = (CFAssign,[NTNumeric n], NTVar o)
  crux o (TFunctor x as) = (CFUnif x, as, NTVar o)

-- | Given a normalized form and an initial crux, saturate the graph and
--   get a plan for doing so.
plan :: (Crux ModedNT -> [Action])
     -> (PartialPlan -> Action -> Cost)
     -> (FDR, ANFState)
     -> Crux NTV
     -> (Cost, Action)
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
-- Experimental Detritus                                                {{{


testPlanRule x =
 let (fr,anfs) = runNormalize $ normRule (unsafeParse DP.drule x)
     updatePlans = map (\c -> (c, plan possible simpleCost (fr,anfs) c))
       $ filter (\(f,_,_) -> case f of { CFCall f' -> not $ isMath f' ; _ -> False })
       $ eval_cruxes anfs
  in updatePlans

main :: IO ()
main = mapM_ (\(c,(s,p)) -> do
                putStrLn $ show c
                putStrLn $ "SCORE: " ++ show s
                forM_ p (putStrLn . show)
                putStrLn "")
       $ testPlanRule
       -- $ "fib(X) :- fib(X-1) + fib(X-2)"
       $ "path(pair(Y,Z),V) min= path(pair(X,Y),U) + cost(X,Y,Z,U,V)."
       -- $ "goal += f(&pair(Y,Y))." -- 

------------------------------------------------------------------------}}}
