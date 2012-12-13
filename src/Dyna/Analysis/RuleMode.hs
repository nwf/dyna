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

import           Control.Arrow (first)
import           Control.Monad
import qualified Data.ByteString.Char8      as BC
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Set                   as S
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Term.TTerm
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Parser       as DP
import           Dyna.XXX.DataUtils(argmin)
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
              | CFStruct v [v] DFunct
              | CFUnif   v  v
              | CFAssign v  n
              | CFEval   v  v
 deriving (Eq,Ord,Show)

cruxMode :: BindChart -> Crux DVar NTV -> Crux (ModedVar) (ModedNT)
cruxMode c cr = case cr of
  CFCall   o is f -> CFCall   (mv o) (map mv is) f
  CFStruct o is f -> CFStruct (mv o) (map mv is) f
  CFAssign o i    -> CFAssign (mv o) (modedNT c i)
  CFEval   o i    -> CFEval   (mv o) (mv i)
  CFUnif   o i    -> CFUnif   (mv o) (mv i)
 where
  mv = modedVar c

cruxVars :: Crux DVar NTV -> S.Set DVar
cruxVars cr = case cr of
  CFCall   o is        _ -> S.fromList (o:is)
  CFStruct o is        _ -> S.fromList (o:is)
  CFAssign o (NTVar i)   -> S.fromList [o,i]
  CFAssign o _           -> S.singleton o
  CFEval   o i           -> S.fromList [o,i]
  CFUnif   o i           -> S.fromList [o,i]

cruxIsEval :: Crux a b -> Bool
cruxIsEval (CFEval _ _)   = True
cruxIsEval (CFCall _ _ _) = True
cruxIsEval _              = False

------------------------------------------------------------------------}}}
-- DOpAMine                                                             {{{

-- | Dyna OPerational Abstract MachINE
--
-- It makes us happy.

--              Opcode          Out         In          Ancillary
data DOpAMine = OPAssign        DVar        NTV                     --  -+
              | OPCheck         DVar        DVar                    --  ++

              -- | Check that two dvars are not equal.  This is used to
              -- prevent double-counting of hyper-edges when any of their
              -- tails can be made to be the same item by specialization.
              -- 
              -- XXX While inspired by Blatz & Eisner 2006, it's unclear
              -- that this is actually what we should be doing.  Oh well,
              -- live and learn.
              | OPCheckNE       DVar        DVar                    --  ++

              | OPGetArgsIf     [DVar]      DVar        DFunct      --  -+
              | OPBuild         DVar        [DVar]      DFunct      --  -+

                -- XXX OPCall and OPIter are actually the same thing,
                --     in the end.  OPCall is just the MF+[MB] variant
                --     of OPIter; at the moment we use them to distinguish
                --     builtins, but that's wrong.
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
               OPCheckNE _ _       -> DetSemi
               OPGetArgsIf _ _ _   -> DetSemi
               OPBuild _ _ _       -> Det
               OPIndirEval _ _     -> DetSemi
               OPCall _ _ _        -> Det
               OPIter o is _       -> -- XXX
                 case (modeOf o, foldr min MBound (map modeOf is)) of
                   (_, MBound) -> DetSemi
                   _           -> DetNon

------------------------------------------------------------------------}}}
-- Actions                                                              {{{

type Action = [DOpAMine]

-- XXX we shouldn't need to know this
--
-- XXX please observe duplication of knowledge with
-- Dyna.Analysis.ANF.dynaFunctorArgDispositions
--
-- XXX Also cross-reference python backend
isMath f = f `elem` [ "^", "+", "-", "*", "/", "&", "|", "~"
                    , "%", "**", "<", ">", "<<", ">>"
                    , "log", "exp", "and", "or", "not"]

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

    -- Structure building
  CFStruct o is funct ->
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

    -- Unification
  CFUnif (MF _) (MF _) -> []
  CFUnif (MB x) (MB y) -> [[OPCheck x y]]
  CFUnif (MB x) (MF y) -> [[OPAssign y (NTVar x)]]
  CFUnif (MF y) (MB x) -> [[OPAssign y (NTVar x)]]

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
  inv "+" [(MB x), (MF y)] (MB o)
                  = [[ OPCall y [o,x] "-" ]]

  inv "+" [(MF x), (MB y)] (MB o)
                  = [[ OPCall x [o,y] "-" ]]

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
unif_cruxes (AS { as_assgn = assigns, as_unifs = unifs }) =
     M.foldrWithKey (\o i -> (crux o i :)) [] assigns
  ++ map (uncurry CFUnif) unifs
 where
  crux :: DVar -> ENF -> Crux DVar NTV
  crux o (Left (NTString s))    = CFAssign o $ NTString s
  crux o (Left (NTNumeric n))   = CFAssign o $ NTNumeric n
  crux o (Left (NTVar i))       = CFAssign o $ NTVar i
  crux o (Right (f,as))         = CFStruct o as f

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
    OPCheck _ _         -> -1 -- Checks are issued with Assigns, so
                              -- counter-act the cost to encourage them
                              -- to be earlier in the plan.
    OPCheckNE _ _       -> 0
    OPGetArgsIf _ _ _   -> 0
    OPBuild _ _ _       -> 1  -- Upweight building due to side-effects
                              -- in the intern table
    OPCall _ _ _        -> 0
    OPIter o is _       -> 2 ** (fromIntegral $ length $ filter isFree (o:is))
                           - 1
    OPIndirEval _ _     -> 100

  loops = fromIntegral . length . filter isLoop

  isLoop :: DOpAMine -> Bool
  isLoop = (== DetNon) . detOfDop

------------------------------------------------------------------------}}}
-- Planning                                                             {{{

data PartialPlan = PP { pp_cruxes         :: S.Set (Crux DVar NTV)
                      , pp_binds          :: BindChart
                      , pp_restrictSearch :: Bool
                      , pp_score          :: Cost
                      , pp_plan           :: Action
                      }

stepPartialPlan ::
                   -- | Possible actions
                   (Crux (ModedVar) (ModedNT) -> [Action])

                   -- | Plan scoring function
                -> (PartialPlan -> Action -> Cost)

                    -- | The 'DFunctAr', intern representation, and
                    -- result variable of the
                    -- initial /evaluation/ crux, if any.  This is used to
                    -- avoid double-counting during updates.
                    --
                    -- Cruxes are implicitly ordered by the name of their
                    -- evaluation variable, so we can easily look to see if
                    -- a given crux is "before" or "after" the initial one
                    -- in this ordering.
                -> Maybe (DFunctAr, DVar, DVar)
                -> PartialPlan
                -> Either (Cost, Action) [PartialPlan]
stepPartialPlan steps score mic p =
  if S.null (pp_cruxes p)
   then Left $ (pp_score p, pp_plan p)
   else Right $
    let rc = pp_cruxes p
    in if pp_restrictSearch p
       -- XXX I am not sure this is right
       --
       --     force consideration of non-evaluation cruxes if
       --     any nonevaluation crux has a possible move.
       --     If a non-evaluation plan exists, commit to its
       --     cheapest choice as the only option here.
       --
       --     This prevents us from considering the multitude
       --     stupid plans that begin by evaluating when they
       --     don't have to.
       then case step (S.filter (not . cruxIsEval) rc) of
              [] -> step (S.filter cruxIsEval rc)
              xs -> [argmin (flip score []) xs]
       else step rc
 where
   step = S.fold (\crux ps -> (
                  let bc = pp_binds p
                      pl = pp_plan p
                      plans = steps (cruxMode bc crux)
                      bc' = bc `S.union` cruxVars crux
                      rc' = S.delete crux (pp_cruxes p)
                      r'  = (not $ cruxIsEval crux) || (pp_restrictSearch p)
                  in map (\act -> let act' = handleConflictors act
                                  in PP rc' bc' r' (score p act') (pl ++ act'))
                         plans
                   ) ++ ps
                ) []

   handleConflictors =
     case mic of
       Nothing -> id
       Just ((f,a),i,ov) -> concatMap $ \dop ->
         case dop of
           OPIter ov' ivs' f' |  f' == f
                              && length ivs' == a
                              && ov > varOfMV ov'
                              -> let cv = "_chk"
                                 in [ dop
                                    , OPBuild cv (map varOfMV ivs') f'
                                    , OPCheckNE i cv
                                    ]
           _ -> [dop]

stepAgenda st sc mic = go [] . (\x -> [x])
 where
  go [] []     = []
  go (r:rs) [] = go rs r
  go rs (p:ps) = case stepPartialPlan st sc mic p of
                    Left df -> df : (go rs ps)
                    Right ps' -> go (ps':rs) ps

-- XXX we're going to need to initially plan a unification crux as part of
-- backward chaining, but we don't yet.
initializeForCrux :: (Crux DVar a, DVar, DVar)
                  -> ((DFunctAr, DVar, DVar), Action)
initializeForCrux (cr, hi, v) = case cr of
  CFCall o is f -> ( ((f,length is), hi, o)
                   , [ OPGetArgsIf is hi f, OPAssign o (NTVar v) ])
  _             -> sorryDynac "Don't know how to initially plan !CFCall"

-- | Given a normalized form and an initial crux, saturate the graph and
--   get a plan for doing so.
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
      (mic,ip) = maybe (Nothing, []) (first Just . initializeForCrux) mi
      initPlan = PP { pp_cruxes = maybe id (\(c,_,_) -> S.delete c) mi
                                  $ S.fromList cruxes
                    , pp_binds  = maybe S.empty (\(c,_,_) -> cruxVars c) mi
                    , pp_restrictSearch = False
                    , pp_score  = 0
                    , pp_plan   = ip
                    }
  in stepAgenda st sc mic initPlan

plan :: (Crux (ModedVar) (ModedNT) -> [Action])
     -> (PartialPlan -> Action -> Cost)
     -> ANFState
     -> Maybe (Crux DVar NTV, DVar, DVar)
     -> Maybe (Cost, Action)
plan st sc anf mi =
  (\x -> case x of
                [] -> Nothing
                plans -> Just $ argmin fst plans)
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

planEachEval_ hi v (FRule { fr_anf = anf })  =
  map (\(c,fa) -> (fa, plan_ possible simpleCost anf $ Just (c,hi,v)))
    $ MA.mapMaybe (\c -> case c of
                           CFCall _ is f | not $ isMath f
                                         -> Just $ (c,(f,length is))
                           _             -> Nothing )
    $ eval_cruxes anf



testPlanRule x = planEachEval_ "HEAD" "VALUE" $ normRule (unsafeParse DP.drule x)

run = mapM_ (\(c,msp) -> do
                putStrLn $ show c
                case msp of
                  []  -> putStrLn "NO PLAN"
                  sps -> forM_ sps $ \(s,p) -> do
                                        putStrLn $ "\n\nSCORE: " ++ show s
                                        forM_ p (putStrLn . show)
                putStrLn "")
       . testPlanRule

------------------------------------------------------------------------}}}
