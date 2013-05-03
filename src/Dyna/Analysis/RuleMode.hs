---------------------------------------------------------------------------
-- | Mode analysis of a rule
--
-- Takes input from "Dyna.Analysis.ANF"
--
-- XXX Gotta start somewhere.

-- Header material                                                      {{{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dyna.Analysis.RuleMode {- (
    Mode(..), Moded(..), ModedNT, isBound, isFree,

    Crux, EvalCrux(..), UnifCrux(..),

    Action, Cost, Det(..),
    BackendPossible, 
 
    planInitializer, planEachEval, planGroundBackchain,

    UpdateEvalMap, combineUpdatePlans,

    QueryEvalMap, combineQueryPlans,

    adornedQueries
) -} where

import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Identity
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
-- import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.Mode.Execution.NoAliasContext
import           Dyna.Analysis.Mode.Execution.NoAliasFunctions
import           Dyna.Term.TTerm
import           Dyna.Term.Normalized
import           Dyna.Main.Exception
import           Dyna.XXX.DataUtils(argmin,mapInOrApp)
import           Dyna.XXX.MonadContext
import           Dyna.XXX.Trifecta (prettySpanLoc)
-- import           Dyna.XXX.TrifectaTest
import           Text.PrettyPrint.Free

-- XXX Aaaaany second now
-- import           Dyna.Analysis.Mode.Execution.Base
-- import           Dyna.Analysis.Mode.Execution.Functions

------------------------------------------------------------------------}}}
-- Bindings                                                             {{{

-- | For variables that are bound, what are they, and all that?
type BindChart = SIMCtx DFunct

type BindM m = EitherT UnifFail (SIMCT m DFunct)

{-
varMode :: BindChart -> DVar -> DInst
varMode c v = maybe (error "BindChart miss") id $ M.lookup v c

modedVar :: BindChart -> DVar -> ModedVar
modedVar b x = case varMode b x of
                 MBound -> MB x
                 MFree  -> MF x

modedNT :: BindChart -> NTV -> ModedNT
modedNT b (NTVar  v)     = NTVar $ modedVar b v
modedNT _ (NTBase b)     = NTBase b
-}

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data EvalCrux v = CFCall v [v] DFunct
                | CFEval v v
 deriving (Eq,Ord,Show)

data UnifCrux v n = CFStruct v [v] DFunct
                  | CFAssign v n
 deriving (Eq,Ord,Show)

type Crux v n = Either (EvalCrux v) (UnifCrux v n)

cruxIsEval :: Crux v n -> Bool
cruxIsEval (Left _) = True
cruxIsEval (Right _) = False

{-
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
-}

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

type Actions fbs = [DOpAMine fbs]

data BackendAction fbs = BAct
                          { bact_dop     :: Actions fbs
                          , bact_outmode :: [(DVar,NIX DFunct)]
                          }
 deriving (Show)


-- | Builtin support hook for mode planning.  Options are
--   to return
-- 
--   * @Left False@  -- This functor is not built in
--
--   * @Left True@   -- There is no plan for this mode
--
--   * @Right act@   -- This operation may run according to the plan given.
--
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
                           -> Either Bool (BackendAction fbs)

{-
mapMaybeModeCompat mis mo =
  MA.mapMaybe (\(is',o',d,f) -> do
                guard $    modeOf mo <= o'
                        && length mis == length is'
                        && and (zipWith (\x y -> modeOf x <= y) mis is')
                return (d,f))
-}

-- | Free, Universal, or Panic.  A rather simplistic take on unification.
--
-- XXX There is nothing good about this.
fup :: forall a m . (Monad m, MCVT m DVar ~ VR DFunct (NIX DFunct), MCR m DVar)
    => DVar -> m a -> m a -> m a
fup v cf cu = do
  vr <- clookup v
  let vi = case vr of
          VRName   vn -> fmap VRName $ nExpose vn
          VRStruct vx -> vx
  case vi of
    IFree   -> cf
    IUniv _ -> cu
    _       -> dynacPanic "Unexpected instantiation state while planning"

possible :: (Monad m)
         => BackendPossible fbs
         -> Crux DVar NTV
         -> SIMCT m DFunct (Actions fbs)
possible fp cr =
  case cr of
      -- XXX Indirect evaluation is not yet supported
    Left (CFEval _ _) -> dynacSorry "Indir eval"

	-- XXX This is going to be such a pile.  We really, really should have
	-- unification crank out a series of DOpAMine opcodes for us, but for
	-- the moment, since everything we do is either IFree or IUniv, just use
	-- iEq everywhere.

    -- Assign or check
    Right (CFAssign o i) ->
        case i of
          NTVar  v -> fup v (fup o (throwError UFExDomain)
                                   (runReaderT (unifyVV v o) (UnifParams True False) >> return [ OPAsgn v (NTVar o) ]))
                            (fup o (runReaderT (unifyVV v o) (UnifParams True False) >> return [ OPAsgn o i ])
                               (return [ OPCheq o v ]))
          NTBase b -> fup o (runReaderT (unifyVU o) (UnifParams True False) >> return [ OPAsgn o i ])
                            (let chk = "_chk" in return [ OPAsgn chk i, OPCheq chk o])

    -- Structure building or unbuilding
    Right (CFStruct o is funct) -> fup o (mapM_ isBound is >> bind o >> return [ OPWrap o is funct ])
                                         (buildPeel)
      where
       buildPeel = do
                    (is', mcis) <- zipWithM maybeCheck is newvars >>= return . unzip
                    let cis = MA.catMaybes mcis
                    return ([ OPPeel is' o funct ] ++ map (uncurry OPCheq) cis)

       newvars = map (\n -> BC.pack $ "_chk_" ++ (show n)) [0..]

       maybeCheck v nv = fup v (return (v,Nothing)) (return (nv, Just (nv,v)))
                            
    Left (CFCall vo vis funct) -> do
      is <- mapM mkMV vis 
      o  <- mkMV vo
      case fp (funct,is,o) of
  		-- Not a built-in, so we assume that it can be iterated in full.
        Left False      -> do mapM_ bind (vo:vis)
                              return [OPIter o is funct DetNon Nothing]
        Left True        -> throwError UFExDomain
        Right (BAct a m) -> do runReaderT
                                 (mapM_ (uncurry $ flip unifyUnaliasedNV) m)
                                 (UnifParams True True) -- XXX Live?
                               return a
 where
     mo = nHide (IUniv UShared)
     unifyVU v = unifyUnaliasedNV mo v
     mkMV v = do
       vi <- clookup v
       return $ MV v (vrToNIX vi) mo

     isBound v = fup v (throwError UFExDomain) (return ())
     bind x = runReaderT (unifyVU x) (UnifParams False False)

------------------------------------------------------------------------}}}
-- ANF to Cruxes                                                        {{{

anfVars :: ANFState -> S.Set DVar
anfVars (AS { as_evals = evals, as_unifs = unifs, as_assgn = assgns } ) =
  S.unions [ M.foldWithKey (\k v s -> S.insert k (go1 v s)) S.empty evals
           , M.foldWithKey (\k v s -> S.insert k (go2 v s)) S.empty assgns
           , foldr (\(v1,v2) s -> S.insert v1 (S.insert v2 s)) S.empty unifs
           ]
  where
   go s (_,vs) = S.union s (S.fromList vs)
   go1 e s = either (flip S.insert s) (go s) e
   go2 e s = either (const s) (go s) e


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
  crux :: DVar -> EBF -> UnifCrux DVar NTV
  crux o (Left  x)              = CFAssign o (NTBase x)
  crux o (Right (f,as))         = CFStruct o as f

------------------------------------------------------------------------}}}
-- Costing Plans                                                        {{{

type Cost = Double

-- XXX I don't understand why this heuristic works, but it seems to exclude
-- some of the... less intelligent plans.
simpleCost :: PartialPlan fbs -> Actions fbs -> Cost
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
                             DetNon  -> 2 {- ** (fromIntegral $ length $
                                              filter isFree (o:is))
                                        - 1 -}
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
                          , pp_plan           :: Actions fbs
                          }

pp_liveVars :: PartialPlan fbs -> S.Set DVar
pp_liveVars p = S.unions $ map lvs $ S.toList (pp_cruxes p)
 where
  lvs (Left  (CFCall   v vs _))       = S.fromList (v:vs)
  lvs (Left  (CFEval   v v'))         = S.fromList [v,v']
  lvs (Right (CFStruct v vs _))       = S.fromList (v:vs)
  lvs (Right (CFAssign v (NTVar v'))) = S.fromList [v,v']
  lvs (Right (CFAssign v (NTBase _))) = S.singleton v

-- XXX This does not have a way to signal UFNotReached back to its caller.
-- That is particularly disappointing since any unification producing that
-- means that there's certainly no plan for the whole rule.
stepPartialPlan :: (Crux DVar NTV -> SIMCT Identity DFunct (Actions fbs))
                -- ^ Possible actions
                -> (PartialPlan fbs -> Actions fbs -> Cost)
                -- ^ Plan scoring function
                -> Maybe (Maybe DFunctAr, DVar, DVar)
                -- ^ The 'DFunctAr', intern representation, and
                -- result variable of the
                -- initial /evaluation/ crux, if any.  This is used to
                -- avoid double-counting during updates.  See $dupcrux
                -> PartialPlan fbs
                -> Either (Cost, Actions fbs) [PartialPlan fbs]
stepPartialPlan poss score mic p =
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
                      plan = runIdentity $ runSIMCT (poss crux) bc
                      rc' = S.delete crux (pp_cruxes p)
                      r'  = (not $ cruxIsEval crux) || (pp_restrictSearch p)
                  in either (const ps)
                            (\(act,bc') -> let act' = handleConflictors act
                                           in PP rc' bc' r' (score p act') (pl ++ act')
                                              : ps)
                            plan
                ) []

   handleConflictors =
     case mic of
       Nothing -> id
       Just (mfa,i,ov) -> \p -> flip concatMap p (\dop ->
         case dop of
           OPIter ov' ivs' f' _ _ |  
                -- We must insert checks whenever this step involves
                -- an evaluation.  As an easy optimisation, if we know
                -- the 'DFunctAr' being updated, we can elide this check
                -- when we're evaluating a different 'DFunctAr'.
                --
                -- XXX This is not the whole answer since it continues to
                -- assume that everything is bound on the way out of an
                -- OPIter.  Really we should be transforming the ANF to
                -- include cruxes for these checks, that way they will get
                -- handled by mode analysis as with everything else.
                (maybe True (== (f',length ivs')) mfa)
             && ov > ov'^.mv_var
             -> let cv = "_chk"
                in [ dop
                   , OPWrap cv (fmap (^.mv_var) ivs') f'
                   , OPCkne i cv
                   ]
           _ -> [dop])

planner_ :: (Crux DVar NTV -> SIMCT Identity DFunct (Actions fbs))
         -- ^ Available steps
         -> (PartialPlan fbs -> Actions fbs -> Cost)
         -- ^ Scoring function
         -> S.Set (Crux DVar NTV)
         -- ^ Cruxes to be planned over
         -> Maybe (EvalCrux DVar, DVar, DVar)
         -- ^ Maybe the updated evaluation crux, the interned
         -- representation of the term being updated, and
         -- result variable.
         -> S.Set DVar
         -- ^ Any variables bound on the way in, in addition to
         --   the two given for an initial crux
         -> S.Set DVar
         -- ^ Unbound variables in the rule
         -> [(Cost, Actions fbs)]
         -- ^ Plans and their costs
planner_ st sc cr mic bv fv = runAgenda
   $ PP { pp_cruxes = cr
        , pp_binds  = SIMCtx $ M.fromSet (const $ VRStruct (IUniv UShared)) (S.unions [bv,bi])
                               `M.union`
                               M.fromSet (const $ VRStruct IFree) fv
        , pp_restrictSearch = False
        , pp_score  = 0
        , pp_plan   = ip
        }
 where
  runAgenda = go [] . (\x -> [x])
   where
    go [] []     = []
    go (r:rs) [] = go rs r
    go rs (p:ps) = case stepPartialPlan st sc mic' p of
                     Left df -> df : (go rs ps)
                     Right ps' -> go (ps':rs) ps

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

bestPlan :: [(Cost, a)] -> Maybe (Cost, a)
bestPlan []    = Nothing
bestPlan plans = Just $ argmin fst plans

-- | Given a normalized form and, optionally, an initial crux,
--   saturate the graph and get all the plans for doing so.
--
-- XXX This has no idea what to do about non-range-restricted rules.
planUpdate_ :: BackendPossible fbs                         -- ^ Available steps
            -> (PartialPlan fbs -> Actions fbs -> Cost)    -- ^ Scoring function
            -> ANFState                                    -- ^ Normal form
            -> Maybe (EvalCrux DVar, DVar, DVar)           -- ^ Initial eval crux
            -> S.Set DVar
            -> [(Cost, Actions fbs)]                       -- ^ If there's a plan...
planUpdate_ bp sc anf mic fv = anfPlanner_ (possible bp) sc anf mic S.empty fv

planUpdate :: BackendPossible fbs
           -> (PartialPlan fbs -> Actions fbs -> Cost)
           -> ANFState
           -> Maybe (EvalCrux DVar, DVar, DVar)
            -> S.Set DVar
           -> Maybe (Cost, Actions fbs)
planUpdate bp sc anf mi fv =
  bestPlan $ planUpdate_ bp sc anf mi fv

planInitializer :: BackendPossible fbs -> Rule -> Maybe (Cost, Actions fbs)
planInitializer bp (Rule { r_anf = anf }) =
  planUpdate bp simpleCost anf Nothing (anfVars anf)

planEachEval :: BackendPossible fbs     -- ^ The backend's primitive support
             -> (DFunctAr -> Bool)      -- ^ Indicator for constant function
             -> Rule
             -> [(Maybe DFunctAr, Maybe (Cost, DVar, DVar, Actions fbs))]
planEachEval bp cs r@(Rule { r_anf = anf })  =
  map (\(mfa,cr) -> (mfa, varify $ planUpdate bp simpleCost anf (Just $ mic cr) (anfVars anf)))
    -- Filter out non-constant evaluations
  $ MA.mapMaybe (\ec -> case ec of
                  CFCall _ is f | not (cs (f,length is))
                                -> Just (Just (f,length is), ec)
                  CFCall _ _  _ -> Nothing
                  CFEval _ _    -> Just (Nothing,ec))

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

{-
planGroundBackchain :: BackendPossible fbs
                    -> Rule
                    -> Maybe (Cost, DVar, Action fbs)
planGroundBackchain bp (Rule { r_anf = anf, r_head = h }) =
  varify
  $ bestPlan
  $ anfPlanner_ (possible bp) simpleCost anf Nothing (S.singleton h)
 where
  varify = fmap $ \(c,a) -> (c,h,a)

planBackchains :: BackendPossible fbs
               -> Rule
               -> M.Map [Mode] (Cost, [DVar], Action fbs)
planBackchains bp (Rule { r_anf = anf, r_head = h })
-}

------------------------------------------------------------------------}}}
-- Update plan combination                                              {{{

type UpdateEvalMap fbs = M.Map (Maybe DFunctAr)
                               [(Rule, Cost, DVar, DVar, Actions fbs)]

-- | Return all plans for each functor/arity
--
-- XXX This may still belong elsewhere.
--
-- XXX This guy wants span information; he's got it now use it.
--
-- timv: might want to fuse these into one circuit
--
combineUpdatePlans :: [(Rule,[( Maybe DFunctAr,
                                Maybe (Cost, DVar, DVar, Actions fbs))])]
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
                          <+> group (pretty fa)
                          <+> "in rule at"
                          <+> (prettySpanLoc $ r_span fr)
      Just (c,v1,v2,a) -> go' xs fr ys $ mapInOrApp fa (fr,c,v1,v2,a) m

------------------------------------------------------------------------}}}
-- Backward chaining plan combination                                   {{{

{-
type QueryEvalMap fbs = M.Map (Maybe DFunctAr)
                              [(Rule, Cost, DVar, Action fbs)]

combineQueryPlans :: [(Rule, Maybe (Cost, DVar, Action fbs))]
                   -> QueryEvalMap fbs  
combineQueryPlans = go (M.empty)
 where
  go m []              = m
  go m ((fr,mcva):xs)  = go' xs fr mcva m

  go' _  fr Nothing        _ = dynacUserErr
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
-}


------------------------------------------------------------------------}}}
-- Adorned Queries                                                      {{{

{-
-- XXX We really ought to be returning something about math, as well, but
-- as all that's handled specially up here...
adornedQueries :: Action fbs -> S.Set (DFunct,[Mode],Mode)
adornedQueries = go S.empty
 where
  go x []                   = x
  go x ((OPIter o is f _ _):as) =
    go (x `S.union` S.singleton (f, map modeOf is, modeOf o)) as
  go x (_:as)               = go x as
-}

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
