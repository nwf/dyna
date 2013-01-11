---------------------------------------------------------------------------
-- | Mode analysis of a rule
--
-- Takes input from "Dyna.Analysis.ANF"
--
-- XXX Gotta start somewhere.

-- Header material                                                      {{{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dyna.Analysis.RuleMode (
    Mode(..), Moded(..), ModedNT, isBound, isFree,

    Crux, EvalCrux(..), UnifCrux(..),

    Action, Cost, Det(..),
    BackendPossible, 
 
    planInitializer, planEachEval, planGroundBackchain,

    UpdateEvalMap, combineUpdatePlans,

	QueryEvalMap, combineQueryPlans,

    adornedQueries
) where

import           Control.Monad
import qualified Data.ByteString.Char8      as BC
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Set                   as S
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.Base
import           Dyna.Term.TTerm
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Parser       as DP
import           Dyna.XXX.DataUtils(argmin,mapInOrApp)
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           Dyna.XXX.TrifectaTest
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Bindings                                                             {{{

-- | What things have thus far been bound under the plan?
type BindChart = S.Set DVar

varMode :: BindChart -> DVar -> Mode
varMode c v = if v `S.member` c then MBound else MFree

modedVar :: BindChart -> DVar -> ModedVar
modedVar b x = case varMode b x of
                 MBound -> MB x
                 MFree  -> MF x

modedNT :: BindChart -> NTV -> ModedNT
modedNT b (NTVar v)     = NTVar $ modedVar b v
modedNT _ (NTBool b)    = NTBool b
modedNT _ (NTString s)  = NTString s
modedNT _ (NTNumeric x) = NTNumeric x

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data EvalCrux v = CFCall v [v] DFunct
                | CFEval v v
 deriving (Eq,Ord,Show)

data UnifCrux v n = CFStruct v [v] DFunct
                  | CFAssign v n
 deriving (Eq,Ord,Show)

type Crux v n = Either (EvalCrux v) (UnifCrux v n)

cruxIsEval (Left _) = True
cruxIsEval (Right _) = False

cruxMode :: BindChart -> Crux DVar NTV -> Crux (ModedVar) (ModedNT)
cruxMode c cr = either (Left . evalMode) (Right . unifMode) cr
 where
  evalMode ec = case ec of
    CFCall   o is f -> CFCall   (mv o) (map mv is) f
    CFEval   o i    -> CFEval   (mv o) (mv i)
  unifMode uc = case uc of
    CFStruct o is f -> CFStruct (mv o) (map mv is) f
    CFAssign o i    -> CFAssign (mv o) (modedNT c i)
  mv = modedVar c

cruxVars :: Crux DVar NTV -> S.Set DVar
cruxVars = either evalVars unifVars
 where
  evalVars cr = case cr of
    CFCall   o is        _ -> S.fromList (o:is)
    CFEval   o i           -> S.fromList [o,i]
  unifVars cr = case cr of
    CFStruct o is        _ -> S.fromList (o:is)
    CFAssign o (NTVar i)   -> S.fromList [o,i]
    CFAssign o _           -> S.singleton o

------------------------------------------------------------------------}}}
-- Actions                                                              {{{

type Action fbs = [DOpAMine fbs]

-- XXX Is this really the right type?
--
-- Note that there's a big wad of complexity here: we want to announce, for
-- example, that predicates may support generation (full-minus moding) but
-- that they can take bound variables, or that some predicates may insist
-- upon being run in mode minus, necessitating the insertion of checks.
-- (Primitives, unfortunately, fall into the latter category!  We have to
-- run +/2 and then check the output, for example).  Right now, the backend
-- is responsible for dealing with the check insertions.  That might be
-- wrong.
type BackendPossible fbs = (DFunct,[ModedVar],ModedVar)
                           -> Either Bool (Action fbs)

type Possible fbs        = (Crux (ModedVar) (ModedNT)) -> Maybe (Action fbs)

{-
mapMaybeModeCompat mis mo =
  MA.mapMaybe (\(is',o',d,f) -> do
                guard $    modeOf mo <= o'
                        && length mis == length is'
                        && and (zipWith (\x y -> modeOf x <= y) mis is')
                return (d,f))
-}

possible :: BackendPossible fbs -> Possible fbs
possible fp cr = case cr of
    -- XXX Indirect evaluation is not yet supported
  Left (CFEval _ _) -> dynacSorry "Indir eval"

    -- Assign or check
  Right (CFAssign o i) ->
      let ni = ntvOfMNT i in
      case (evnOfMNT i, o) of
        (Left _, MF _)   -> Nothing
        (Right _, MB o') -> let chk = "_chk" in
                            Just [ OPAsgn chk ni
                                 , OPCheq chk o']
        (Left i', MB o') -> Just [OPAsgn i' (NTVar o')]
        (Right _, MF o') -> Just [OPAsgn o' ni]

    -- Structure building
  Right (CFStruct o is funct) ->
      case o of
        -- If the output is free, the only supported case is when all
        -- inputs are known.
        MF o'  -> if all isBound is
                   then Just [OPWrap o' (map varOfMV is) funct]
                   else Nothing
        -- On the other hand, if the output is known, then any subset
        -- of the inputs may be known and will be checked.
        MB o' -> Just $   (OPPeel is' o' funct)
                        : map (\(c,x) -> (OPCheq c x)) cis
         where
          mkChks _ (MF i) = (i, Nothing)
          mkChks n (MB v) = let chk = BC.pack $ "_chk_" ++ (show n)
                            in (chk, Just (chk, v))

          (is',mcis) = unzip $ zipWith mkChks [0::Int ..] is
          cis        = MA.catMaybes mcis

  Left (CFCall o is funct) ->
    case fp (funct,is,o) of
      Left False  -> Just [OPIter o is funct DetNon Nothing ]
      Left True   -> Nothing
      Right a     -> Just a

------------------------------------------------------------------------}}}
-- ANF to Cruxes                                                        {{{

eval_cruxes :: ANFState -> [EvalCrux DVar]
eval_cruxes = M.foldrWithKey (\o i -> (crux o i :)) [] . as_evals
 where
  crux :: DVar -> EVF -> EvalCrux DVar
  crux o (Left v) = CFEval o v
  crux o (Right (f,as)) = CFCall o as f

unif_cruxes :: ANFState -> [UnifCrux DVar NTV]
unif_cruxes (AS { as_assgn = assigns, as_unifs = unifs }) =
     M.foldrWithKey (\o i -> (crux o i :)) [] assigns
  ++ map (\(v1,v2) -> CFAssign v1 (NTVar v2)) unifs
 where
  crux :: DVar -> ENF -> UnifCrux DVar NTV
  crux o (Left  x)              = CFAssign o x
  crux o (Right (f,as))         = CFStruct o as f

------------------------------------------------------------------------}}}
-- Costing Plans                                                        {{{

type Cost = Double

-- XXX I don't understand why this heuristic works, but it seems to exclude
-- some of the... less intelligent plans.
simpleCost :: PartialPlan fbs -> Action fbs -> Cost
simpleCost (PP { pp_score = osc, pp_plan = pfx }) act =
    2 * osc + (1 + loops pfx) * actCost act
 where
  actCost = sum . map stepCost

  stepCost :: DOpAMine fbs -> Double
  stepCost x = case x of
    OPAsgn _ _          -> 1
    OPCheq _ _          -> -1 -- Checks are issued with Assigns, so
                              -- counter-act the cost to encourage them
                              -- to be earlier in the plan.
    OPCkne _ _          -> 0
    OPPeel _ _ _        -> 0
    OPWrap _ _ _        -> 1  -- Upweight building due to side-effects
                              -- in the intern table
    OPIter o is _ d _   -> case d of
                             Det     -> 0
                             DetSemi -> 1
                             DetNon  -> 2 ** (fromIntegral $ length $
                                              filter isFree (o:is))
                                        - 1
    OPIndr _ _          -> 100

  loops = fromIntegral . length . filter isLoop

  isLoop :: DOpAMine fbs -> Bool
  isLoop = (== DetNon) . detOfDop

------------------------------------------------------------------------}}}
-- Planning                                                             {{{

-- $dupcrux
--
-- Consider a rule like @a += b(X) * b(Y).@  This desugars into an ANF with
-- two separate evaluations of @b(_)@.  This is problematic, since we will
-- plan each evaluation separately.  (Note that CSE won't help; we really do
-- mean to compute the cross-product in this case, but not double-count the
-- diagonal!)  The workaround here is to /order/ the evaluations (by their
-- ANF temporary variables, for the moment).
--
-- For replacement updates, the correct action is to @continue@ the
-- evaluation loop when an eariler (by the intrinsic ordering) iterator
-- within a update to a later (by the intrinsic ordering) evaluation
-- lands at the same position.
--
-- For delta updates, the ordering is used for the Blatz-Eisner update
-- propagation strategy -- new values are used in earlier evaluations (than
-- the one being updated) and old values are used in later evaluations.
--
-- When backward chaining, we get to ignore all of this, since we only
-- produce one backward chaining plan.
--
-- XXX It's unclear that this is really the right solution.  Maybe we should
-- be planning a single stream of instructions for each dfuctar, rather than
-- each evalution arc, but it's not quite clear that there's a nice
-- graphical story to be told in that case?
--
-- XXX What do we do in the CFEval case??  We need to check every evaluation
-- inside a CFEval update?

data PartialPlan fbs = PP { pp_cruxes         :: S.Set (Crux DVar NTV)
                          , pp_binds          :: BindChart
                          , pp_restrictSearch :: Bool
                          , pp_score          :: Cost
                          , pp_plan           :: Action fbs
                          }

stepPartialPlan ::
                   -- | Possible actions
                   Possible fbs

                   -- | Plan scoring function
                -> (PartialPlan fbs -> Action fbs -> Cost)

                    -- | The 'DFunctAr', intern representation, and
                    -- result variable of the
                    -- initial /evaluation/ crux, if any.  This is used to
                    -- avoid double-counting during updates.  See $dupcrux
                -> Maybe (Maybe DFunctAr, DVar, DVar)
                -> PartialPlan fbs
                -> Either (Cost, Action fbs) [PartialPlan fbs]
stepPartialPlan steps score mic p =
  -- XT.traceShow ("SPP", mic, pp_binds p, pp_cruxes p) $
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
   step = S.fold (\crux ps ->
                  let bc = pp_binds p
                      pl = pp_plan p
                      plan = steps (cruxMode bc crux)
                      bc' = bc `S.union` cruxVars crux
                      rc' = S.delete crux (pp_cruxes p)
                      r'  = (not $ cruxIsEval crux) || (pp_restrictSearch p)
                  in maybe ps
                           (\act -> let act' = handleConflictors act
                                    in PP rc' bc' r' (score p act') (pl ++ act')
                                       : ps)
                           plan
                ) []

   handleConflictors =
     case mic of
       Nothing -> id
       Just (mfa,i,ov) -> concatMap $ \dop ->
         case dop of
           OPIter ov' ivs' f' _ _ |  
				-- We must insert checks whenever this step involves
				-- an evaluation.  As an easy optimisation, if we know
				-- the 'DFunctAr' being updated, we can elide this check
				-- when we're evaluating a different 'DFunctAr'.
                (maybe True (== (f',length ivs')) mfa)
             && ov > varOfMV ov'
             -> let cv = "_chk"
                in [ dop
                   , OPWrap cv (map varOfMV ivs') f'
                   , OPCkne i cv
                   ]
           _ -> [dop]

stepAgenda st sc mic = go [] . (\x -> [x])
 where
  go [] []     = []
  go (r:rs) [] = go rs r
  go rs (p:ps) = case stepPartialPlan st sc mic p of
                    Left df -> df : (go rs ps)
                    Right ps' -> go (ps':rs) ps

planner_ :: -- | Available steps
            Possible fbs                                
            -- | Scoring function
         -> (PartialPlan fbs -> Action fbs -> Cost)
            -- | Cruxes to be planned over
         -> S.Set (Crux DVar NTV)
            -- | Maybe the updated evaluation crux, the interned
            -- representation of the term being updated, and
            -- result variable.
         -> Maybe (EvalCrux DVar, DVar, DVar)
            -- | Any variables bound on the way in, in addition to
            --   the two given for an initial crux
         -> S.Set DVar
            -- | Plans and their costs
         -> [(Cost, Action fbs)]
planner_ st sc cr mic bv = stepAgenda st sc mic'
   $ PP { pp_cruxes = cr
        , pp_binds  = S.union bv bi
        , pp_restrictSearch = False
        , pp_score  = 0
        , pp_plan   = ip
        }
 where
  -- XREF:INITPLAN
  (ip,bi,mic') = case mic of
                Nothing -> ([],S.empty,Nothing)
                Just (CFCall o is f, hi, ho) -> ( [ OPPeel is hi f
                                                  , OPAsgn o (NTVar ho)]
                                                , S.fromList $ o:is
                                                , Just (Just (f,length is),o,hi))
                Just (CFEval o i, hi, ho) -> ( [ OPAsgn i (NTVar hi)
                                               , OPAsgn o (NTVar ho)]
                                             , S.fromList $ [o,i] 
                                             , Just (Nothing,o,i))

anfPlanner_ st sc anf mic bv = planner_ st sc cruxes mic bv
 where
  cruxes =           S.fromList (map Right $ unif_cruxes anf)
           `S.union` ( S.map Left
                       $ maybe id (\(ic,_,_) -> S.delete ic) mic
                       $ S.fromList $ eval_cruxes anf)

bestPlan []    = Nothing
bestPlan plans = Just $ argmin fst plans

-- | Given a normalized form and, optionally, an initial crux,
--   saturate the graph and get all the plans for doing so.
--
-- XXX This has no idea what to do about non-range-restricted rules.
planUpdate_ :: BackendPossible fbs                         -- ^ Available steps
            -> (PartialPlan fbs -> Action fbs -> Cost)     -- ^ Scoring function
            -> ANFState                                    -- ^ Normal form
            -> Maybe (EvalCrux DVar, DVar, DVar)           -- ^ Initial eval crux
            -> [(Cost, Action fbs)]                        -- ^ If there's a plan...
planUpdate_ bp sc anf mic = anfPlanner_ (possible bp) sc anf mic S.empty

planUpdate :: BackendPossible fbs
           -> (PartialPlan fbs -> Action fbs -> Cost)
           -> ANFState
           -> Maybe (EvalCrux DVar, DVar, DVar)
           -> Maybe (Cost, Action fbs)
planUpdate bp sc anf mi =
  bestPlan $ planUpdate_ bp sc anf mi

planInitializer :: BackendPossible fbs -> Rule -> Maybe (Cost, Action fbs)
planInitializer bp (Rule { r_anf = anf }) =
  planUpdate bp simpleCost anf Nothing

planEachEval :: BackendPossible fbs     -- ^ The backend's primitive support
             -> (DFunctAr -> Bool)      -- ^ Indicator for constant function
             -> Rule
             -> [(Maybe DFunctAr, Maybe (Cost, DVar, DVar, Action fbs))]
planEachEval bp cs (Rule { r_anf = anf })  =
  map (\(mfa,cr) -> (mfa, varify $ planUpdate bp simpleCost anf $ Just $ mic cr))
	-- Filter out non-constant evaluations
  $ MA.mapMaybe (\ec -> case ec of
                  CFCall _ is f | not (cs (f,length is))
                                -> Just (Just (f,length is), ec)
                  CFCall _ _  _ -> Nothing
                  CFEval o i    -> Just (Nothing,ec))

	-- Grab all evaluations
  $ eval_cruxes anf
 where
	-- XXX I am not terribly happy about these, but it'll do for the moment.
	--
	-- If the mechanism of feeding updates into these plans is to change,
	-- please ensure that XREF:INITPLAN also changes appropriately.
  varify = fmap $ \(c,a) -> (c,varHead,varVal,a)
  mic x = (x,varHead,varVal)
  varHead = "__i"
  varVal  = "__v"

planGroundBackchain :: BackendPossible fbs
                    -> Rule
                    -> Maybe (Cost, DVar, Action fbs)
planGroundBackchain bp (Rule { r_anf = anf, r_head = h }) =
  varify
  $ bestPlan
  $ anfPlanner_ (possible bp) simpleCost anf Nothing (S.singleton h)
 where
  varify = fmap $ \(c,a) -> (c,h,a)

{-
planBackchains :: BackendPossible fbs
               -> Rule
               -> M.Map [Mode] (Cost, [DVar], Action fbs)
planBackchains bp (Rule { r_anf = anf, r_head = h })
-}

------------------------------------------------------------------------}}}
-- Update plan combination                                              {{{

type UpdateEvalMap fbs = M.Map (Maybe DFunctAr)
                               [(Rule, Cost, DVar, DVar, Action fbs)]

-- | Return all plans for each functor/arity
--
-- XXX This may still belong elsewhere.
--
-- XXX This guy wants span information; he's got it now use it.
--
-- timv: might want to fuse these into one circuit
--
combineUpdatePlans :: [(Rule,[( Maybe DFunctAr,
                                Maybe (Cost, DVar, DVar, Action fbs))])]
                   -> UpdateEvalMap fbs  
combineUpdatePlans = go (M.empty)
 where
  go m []             = m
  go m ((fr,cmca):xs) = go' xs fr cmca m

  go' xs _  []           m = go m xs
  go' xs fr ((fa,mca):ys) m =
    case mca of
      Nothing -> dynacUserErr
                       $ "No update plan for "
                          <+> (pretty fa)
                          <+> "in rule at"
                          <+> (prettySpanLoc $ r_span fr)
      Just (c,v1,v2,a) -> go' xs fr ys $ mapInOrApp fa (fr,c,v1,v2,a) m

------------------------------------------------------------------------}}}
-- Backward chaining plan combination                                   {{{

type QueryEvalMap fbs = M.Map (Maybe DFunctAr)
                              [(Rule, Cost, DVar, Action fbs)]

combineQueryPlans :: [(Rule, Maybe (Cost, DVar, Action fbs))]
                   -> QueryEvalMap fbs  
combineQueryPlans = go (M.empty)
 where
  go m []              = m
  go m ((fr,mcva):xs)  = go' xs fr mcva m

  go' xs fr Nothing      m = dynacUserErr
                             $ "No query plan for rule at"
                             <+> (prettySpanLoc $ r_span fr)
  go' xs fr (Just (c,v,a)) m = go (mapInOrApp (findHeadFA fr)
                                              (fr,c,v,a)
                                              m)
                                  xs

	-- XXX This is unforunate and wrong, but our ANF is not quite right to
    -- let us do this right.  See also Dyna.Backend.Python's use of this
    -- function.
  findHeadFA (Rule _ h _ _ _ (AS { as_assgn = as })) =
    case M.lookup h as of
      Nothing            -> error "No unification for head variable?"
      Just (Left _)      -> error "NTVar head?"
      Just (Right (f,a)) -> Just (f, length a)



------------------------------------------------------------------------}}}
-- Adorned Queries                                                      {{{

-- XXX We really ought to be returning something about math, as well, but
-- as all that's handled specially up here...
adornedQueries :: Action fbs -> S.Set (DFunct,[Mode],Mode)
adornedQueries = go S.empty
 where
  go x []                   = x
  go x ((OPIter o is f _ _):as) =
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

{-
planEachEval_ hi v (Rule { r_anf = anf })  =
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
-}

------------------------------------------------------------------------}}}
