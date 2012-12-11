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

module Dyna.Analysis.RuleMode (
    Mode(..), Moded(..), ModedNT, isBound, isFree,

    Crux(..),

    DOpAMine(..), detOfDop,

    Action, Cost, Det(..), planInitializer, planEachEval,

    adornedQueries
) where

import           Control.Monad
import qualified Data.ByteString.Char8      as BC
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Ord                   as O
import qualified Data.Set                   as S
import           Dyna.Analysis.ANF
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as DP
import           Dyna.XXX.TrifectaTest

------------------------------------------------------------------------}}}
-- Modes                                                                {{{

data Mode = MBound | MFree deriving (Eq,Ord,Show)

-- | What things have thus far been bound under the plan?
type BindChart = S.Set DVar

varMode :: BindChart -> DVar -> Mode
varMode c v = if v `S.member` c then MBound else MFree

data Moded v = MF DVar
             | MB v
 deriving (Eq,Ord,Show)

modeOf :: Moded a -> Mode
modeOf (MF _) = MFree
modeOf (MB _) = MBound

isBound, isFree :: Moded a -> Bool
isBound = (== MBound) . modeOf
isFree  = (== MFree ) . modeOf

type ModedVar = Moded DVar

modedVar :: BindChart -> DVar -> ModedVar
modedVar b x = case varMode b x of
                 MBound -> MB x
                 MFree  -> MF x

varOfMV :: ModedVar -> DVar
varOfMV (MF x) = x
varOfMV (MB x) = x

type ModedNT = NT (ModedVar)

modedNT :: BindChart -> NTV -> ModedNT
modedNT b (NTVar v)     = NTVar $ modedVar b v
modedNT _ (NTString s)  = NTString s
modedNT _ (NTNumeric x) = NTNumeric x

evnOfMNT :: ModedNT -> Either DVar NTV
evnOfMNT (NTVar mv)    = case mv of
                           MB v -> Right (NTVar v)
                           MF v -> Left  v
evnOfMNT (NTString s)  = Right (NTString s)
evnOfMNT (NTNumeric n) = Right (NTNumeric n)

ntvOfMNT :: ModedNT -> NTV
ntvOfMNT (NTVar mx)    = NTVar $ varOfMV mx
ntvOfMNT (NTString s)  = NTString s
ntvOfMNT (NTNumeric n) = NTNumeric n

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data Crux v n = CFCall   v [v] DFunct
              | CFUnif   v [v] DFunct
              | CFAssign v  n
              | CFEval   v  v
 deriving (Eq,Ord,Show)

cruxMode :: BindChart -> Crux DVar NTV -> Crux (ModedVar) (ModedNT)
cruxMode c cr = case cr of
  CFCall   o is f -> CFCall   (mv o) (map mv is) f
  CFUnif   o is f -> CFUnif   (mv o) (map mv is) f
  CFAssign o i    -> CFAssign (mv o) (modedNT c i)
  CFEval   o i    -> CFEval   (mv o) (mv i)
 where
  mv = modedVar c

cruxVars :: Crux DVar NTV -> S.Set DVar
cruxVars cr = case cr of
  CFCall   o is        _ -> S.fromList (o:is)
  CFUnif   o is        _ -> S.fromList (o:is)
  CFAssign o (NTVar i)   -> S.fromList [o,i]
  CFAssign o _           -> S.singleton o
  CFEval   o i           -> S.fromList [o,i]

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.

--              Opcode          Out         In          Ancillary
data DOpAMine = OPAssign        DVar        NTV                     --  -+
              | OPCheck         DVar        DVar                    --  ++

              | OPGetArgsIf     [DVar]      DVar        DFunct      --  -+
              | OPBuild         DVar        [DVar]      DFunct      --  -+

              | OPCall          DVar        [DVar]      DFunct      --  -+
              | OPIter          (ModedVar)  [ModedVar]  DFunct      --  ??
              | OPIndirEval     DVar        DVar                    --  -+
 deriving (Eq,Ord,Show)

data Det = Det          -- ^ Exactly one answer
         | DetSemi      -- ^ At most one answer
         | DetNon       -- ^ Unknown number of answers
 deriving (Eq,Ord,Show)

detOfDop :: DOpAMine -> Det
detOfDop x = case x of
               OPAssign _ _        -> Det
               OPCheck _ _         -> DetSemi
               OPGetArgsIf _ _ _   -> DetSemi
               OPBuild _ _ _       -> Det
               OPIndirEval _ _     -> DetSemi
               OPCall _ _ _        -> Det
               OPIter o is _       -> -- XXX
                 case (modeOf o, foldr min MBound (map modeOf is)) of
                   (MFree, MBound) -> DetSemi
                   _               -> DetNon

------------------------------------------------------------------------}}}
-- Actions                                                              {{{

type Action = [DOpAMine]

-- XXX we shouldn't need to know this
isMath f = f `elem` ["^", "+", "-", "*", "/"]

-- XXX This function really ought to be generated from some declarations in
-- the source program, rather than hard-coded in quite the way it is.
-- Maybe the knowledge of unification is OK.
possible :: Crux (ModedVar) (ModedNT) -> [Action]
possible cr = case cr of
    -- XXX Indirect evaluation is not yet supported
  CFEval _ _ -> []

    -- Assign or check
  CFAssign o i -> let ni = ntvOfMNT i in
                  case (evnOfMNT i, o) of
                    (Left _, MF _)   -> []
                    (Right _, MB o') -> let chk = "_chk" in
                                       [[ OPAssign chk ni
                                        , OPCheck  chk o']]
                    (Left i', MB o') -> [[OPAssign i' (NTVar o')]]
                    (Right _, MF o') -> [[OPAssign o' ni]]

    -- Unification
  CFUnif o is funct ->
      case o of
        -- If the output is free, the only supported case is when all
        -- inputs are known.
        MF o'  -> if all isBound is
                   then [[OPBuild o' (map varOfMV is) funct]]
                   else []
        -- On the other hand, if the output is known, then any subset
        -- of the inputs may be known and will be checked.
        MB o' -> [   (OPGetArgsIf is' o' funct)
                   : map (\(c,x) -> (OPCheck c x)) cis
                 ]
         where
          mkChks _ (MF i) = (i, Nothing)
          mkChks n (MB v) = let chk = BC.pack $ "_chk_" ++ (show n)
                            in (chk, Just (chk, v))

          (is',mcis) = unzip $ zipWith mkChks [0::Int ..] is
          cis        = MA.catMaybes mcis

    -- Backward-chainable mathematics (this is such a hack XXX)
  CFCall o is funct | isMath funct ->
      if not $ all isBound is
       then inv funct is o
       else let is' = map varOfMV is in
            case o of
              MF o' ->  [[OPCall o' is' funct]]
              MB o' -> let cv = "_chk"
                       in [[OPCall  cv is' funct
                           ,OPCheck cv o'
                           ]]

    -- Otherwise, we assume it's an extensional table and ask to iterate
    -- over it.
  CFCall o is funct | otherwise -> [[OPIter o is funct]]

 where

  -- XXX this really ought to be done some other way
  inv :: DFunct -> [ModedVar] -> ModedVar -> [Action]
  inv "+" is (MB o)
               = case L.partition isFree is of
                   ([MF fi],bis) -> let cv = "_tmp"
                                        bis' = map varOfMV bis
                                    in  [[ OPCall cv bis' "+"
                                         , OPCall fi [o, cv] "-"]]
                   _ -> []

  inv "-" [(MB x),(MF y)] (MB o)
                  = [[ OPCall y [x,o] "-" ]]

  inv "-" [(MF x),(MB y)] (MB o)
                  = [[ OPCall x [o,y] "+" ]]

  inv _   _  _  = []

------------------------------------------------------------------------}}}
-- ANF to Cruxes                                                        {{{

eval_cruxes :: ANFState -> [Crux DVar NTV]
eval_cruxes = M.foldrWithKey (\o i -> (crux o i :)) [] . as_evals
 where
  crux :: DVar -> EVF -> Crux DVar NTV
  crux o (Left v) = CFEval o v
  crux o (Right (f,as)) = CFCall o as f
  -- XXX Missing cases

unif_cruxes :: ANFState -> [Crux DVar NTV]
unif_cruxes = M.foldrWithKey (\o i -> (crux o i :)) [] . as_unifs
 where
  crux :: DVar -> ENF -> Crux DVar NTV
  crux o (Left (NTString s))    = CFAssign o $ NTString s
  crux o (Left (NTNumeric n))   = CFAssign o $ NTNumeric n
  crux o (Left (NTVar i))       = CFAssign o $ NTVar i
  crux o (Right (f,as))         = CFUnif o as f

------------------------------------------------------------------------}}}
-- Costing Plans                                                        {{{

type Cost = Double

-- XXX I don't understand why this heuristic works, but it seems to exclude
-- some of the... less intelligent plans.
simpleCost :: PartialPlan -> Action -> Cost
simpleCost (PP { pp_score = osc, pp_plan = pfx }) act =
    2 * osc + (1 + loops pfx) * actCost act
 where
  actCost = sum . map stepCost

  stepCost :: DOpAMine -> Double
  stepCost x = case x of
    OPAssign _ _        -> 1
    OPCheck _ _         -> 2
    OPGetArgsIf _ _ _   -> 1
    OPBuild _ _ _       -> 1
    OPCall _ _ _        -> 1
    OPIter o is _       -> 2 * (fromIntegral $ length $ filter isFree (o:is))
    OPIndirEval _ _     -> 100

  loops = fromIntegral . length . filter isLoop

  isLoop :: DOpAMine -> Bool
  isLoop = (== DetNon) . detOfDop

------------------------------------------------------------------------}}}
-- Planning                                                             {{{

data PartialPlan = PP { pp_cruxes :: S.Set (Crux DVar NTV)
                      , pp_binds  :: BindChart
                      , pp_score  :: Cost
                      , pp_plan   :: Action
                      }

stepPartialPlan :: (Crux (ModedVar) (ModedNT) -> [Action])
                -> (PartialPlan -> Action -> Cost)
                -> PartialPlan
                -> Either (Cost, Action) [PartialPlan]
stepPartialPlan steps score p =
  if S.null (pp_cruxes p)
   then Left $ (pp_score p, pp_plan p)
   else Right $
    let rc = pp_cruxes p
    in  S.fold (\crux ps -> (
                let bc = pp_binds p
                    pl = pp_plan  p
                    plans = steps (cruxMode bc crux)
                    bc' = bc `S.union` cruxVars crux
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

initialPlanForCrux :: (Crux DVar a, DVar, DVar) -> Action
initialPlanForCrux (cr, hi, v) = case cr of
  CFCall o is f -> [ OPGetArgsIf is hi f, OPAssign o (NTVar v) ]
  _             -> error "Don't know how to initially plan !CFCall"

-- | Given a normalized form and an initial crux, saturate the graph and
--   get a plan for doing so.
--
-- XXX If the intial entrypoint is nonlinear, we need to insert some
-- checks into the plan.  Fixing that is moderately invasive...
--
-- XXX This has no idea what to do about non-range-restricted rules.
plan_ :: (Crux (ModedVar) (ModedNT) -> [Action]) -- ^ Available steps
      -> (PartialPlan -> Action -> Cost)             -- ^ Scoring function
      -> ANFState                                    -- ^ Normal form
      -> Maybe (Crux DVar NTV, DVar, DVar)           -- ^ Initial crux,
                                                     --   item intern, and
                                                     --   value, if any.
      -> [(Cost, Action)]                            -- ^ If there's a plan...
plan_ st sc anf mi =
  let cruxes =    eval_cruxes anf
               ++ unif_cruxes anf
      initPlan = PP { pp_cruxes = maybe id (\(c,_,_) -> S.delete c) mi
                                  $ S.fromList cruxes
                    , pp_binds  = maybe S.empty (\(c,_,_) -> cruxVars c) mi
                    , pp_score  = 0
                    , pp_plan   = maybe [] initialPlanForCrux mi
                    }
  in stepAgenda st sc [initPlan]

plan st sc anf mi =
  (\x -> case x of
                [] -> Nothing
                plans -> Just $ L.minimumBy (O.comparing fst) plans)
  $ plan_ st sc anf mi

planInitializer :: FRule -> Maybe (Cost,Action)
planInitializer (FRule { fr_anf = anf }) = plan possible simpleCost anf Nothing

planEachEval :: DVar -> DVar -> FRule -> [(DFunctAr, Maybe (Cost,Action))]
planEachEval hi v (FRule { fr_anf = anf })  =
  map (\(c,fa) -> (fa, plan possible simpleCost anf $ Just (c,hi,v)))
    $ MA.mapMaybe (\c -> case c of
                           CFCall _ is f | not $ isMath f
                                         -> Just $ (c,(f,length is))
                           _             -> Nothing )
    $ eval_cruxes anf

------------------------------------------------------------------------}}}
-- Adorned Queries                                                      {{{

-- XXX We really ought to be returning something about math, as well, but
-- as all that's handled specially up here...
adornedQueries :: Action -> S.Set (DFunct,[Mode],Mode)
adornedQueries = go S.empty
 where
  go x []                   = x
  go x ((OPIter o is f):as) =
    go (x `S.union` S.singleton (f, map modeOf is, modeOf o)) as
  go x (_:as)               = go x as

------------------------------------------------------------------------}}}
-- Experimental Detritus                                                {{{

{-
filterNTs :: [NT v] -> [v]
filterNTs = MA.mapMaybe isNTVar
 where
  isNTVar (NTVar x) = Just x
  isNTVar _         = Nothing

ntMode :: BindChart -> NTV -> Mode
ntMode c (NTVar v) = varMode c v
ntMode _ (NTString _) = MBound
ntMode _ (NTNumeric _) = MBound
-}

testPlanRule x = planEachEval "HEAD" "VALUE" $ normRule (unsafeParse DP.drule x)

main :: IO ()
main = mapM_ (\(c,msp) -> do
                putStrLn $ show c
                case msp of
                  Nothing  -> putStrLn "NO PLAN"
                  Just sps -> forM_ [sps] $ \(s,p) -> do
                                        putStrLn $ "SCORE: " ++ show s
                                        forM_ p (putStrLn . show)
                putStrLn "")
       $ take 1 $ testPlanRule
       -- "fib(X) :- fib(X-1) + fib(X-2)"
       "path(pair(Y,Z),V) min= path(pair(X,Y),1,U) + cost(X,Y,Z,U,V)."
       -- "goal += f(&pair(Y,Y))."

------------------------------------------------------------------------}}}
