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
{-# OPTIONS_GHC -Wall #-}

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

import           Control.Arrow (second)
import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Identity
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.IntMap                as IM
-- import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.ANFPretty
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.Mode.Execution.NoAliasContext
import           Dyna.Analysis.Mode.Execution.NoAliasFunctions
import           Dyna.Term.TTerm
import           Dyna.Term.Normalized
import           Dyna.Main.Exception
import           Dyna.XXX.DataUtils(argmin,mapInOrCons,mapMinRepView)
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
-- Actions                                                              {{{

type Actions fbs = [DOpAMine fbs]

data BackendAction fbs = BAct
                          { bact_dop     :: Actions fbs

                          -- XXX Does not support aliasing
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

-- | Free, Ground, or Neither.  A rather simplistic take on unification.
--
-- XXX There is nothing good about this.
fgn :: forall a m . (Monad m, MCVT m DVar ~ VR DFunct (NIX DFunct), MCR m DVar)
    => DVar -> m a -> m a -> m a -> m a
fgn v cf cg cn = do
  ff <- v `subVN` (nHide IFree)
  gf <- v `subVN` (nHide $ IUniv UShared)
  case (ff,gf) of
    (True ,True ) -> dynacPanicStr "Variable is both free and ground"
    (True ,False) -> cf
    (False,True ) -> cg
    (False,False) -> cn

possible :: (Monad m)
         => BackendPossible fbs
         -> Rule
         -> Crux DVar TBase
         -> SIMCT m DFunct (Actions fbs)
possible fp r cr =
  case cr of
    -- XXX This is going to be such a pile.  We really, really should have
    -- unification crank out a series of DOpAMine opcodes for us, but for
    -- the moment, since everything we do is either IFree or IUniv, just use
    -- iEq everywhere.

    -- XXX Actually, this is all worse than it should be.  The unification
    -- should be done before any case analysis.  Note that we also don't do
    -- any liveness analysis correctly!

    -- Assign or check
    Right (CAssign o i) ->
        fgn o (runReaderT (unifyVU o) (UnifParams True False)
                >> return [ OPAsgn o (NTBase i) ])
              (let chk = "_chk" in return [ OPAsgn chk (NTBase i), OPCheq chk o])
              (throwError UFExDomain)

    Right (CEquals o i) ->
       fgn o (fgn i (throwError UFExDomain)
                    (runReaderT (unifyVV i o) (UnifParams True False)
                       >> return [ OPAsgn o (NTVar i) ])
                    (throwError UFExDomain))
             (fgn i (runReaderT (unifyVV i o) (UnifParams True False)
                       >> return [ OPAsgn i (NTVar o) ])
                    (return [ OPCheq o i ])
                    (throwError UFExDomain))
             (throwError UFExDomain)

{-
        case i of
          NTVar  v -> fup v (fup o (throwError UFExDomain)
                                   
-}

    -- Structure building or unbuilding
    --
    -- XXX This ought to avail itself of unifyVF but doesn't.
    Right (CStruct o is funct) ->
      fgn o (mapM_ ensureBound is >> bind o >> return [ OPWrap o is funct ])
            buildPeel
            (throwError UFExDomain)
     where
      buildPeel = do
                   (is', mcis) <- zipWithM maybeCheck is newvars >>= return . unzip
                   let cis = MA.catMaybes mcis
                   mapM_ bind is
                   return ([ OPPeel is' o funct ] ++ map (uncurry OPCheq) cis)

      newvars = map (\n -> BC.pack $ "_chk_" ++ (show n)) [0::Int ..]

      maybeCheck v nv = fgn v (return (v,Nothing))
                              (return (nv, Just (nv,v)))
                              (throwError UFExDomain)

    -- Disequality constraints require that both inputs be brought to ground
    Right (CNotEqu o i) -> fgn o (throwError UFExDomain)
                                 (fgn i (throwError UFExDomain)
                                        (return [ OPCkne o i ])
                                        (throwError UFExDomain))
                                 (throwError UFExDomain)

    -- XXX Indirect evaluation is not yet supported
    Left (eix, CEval _ _) -> dynacSorry $ "Indir eval"
                                      <+> parens ("eix=" <> pretty eix)
                                      <+> "in rule at"
                                      </> prettySpanLoc (r_span r)

    -- Evaluation
    Left (_, CCall vo vis funct) -> do
      is <- mapM mkMV vis 
      o  <- mkMV vo
      case fp (funct,is,o) of
          -- XXX Not a built-in, so we assume that it can be iterated in full.
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

     ensureBound v = fgn v (throwError UFExDomain)
                           (return ())
                           (throwError UFExDomain)
     bind x = runReaderT (unifyVU x) (UnifParams False False)

------------------------------------------------------------------------}}}
-- ANF to Cruxes                                                        {{{

{-
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
  crux o (Left v) = CEval o v
  crux o (Right (f,as)) = CCall o as f

unif_cruxes :: ANFState -> [UnifCrux DVar NTV]
unif_cruxes (AS { as_assgn = assigns, as_unifs = unifs }) =
     M.foldrWithKey (\o i -> (crux o i :)) [] assigns
  ++ map (\(v1,v2) -> CAssign v1 (NTVar v2)) unifs
 where
  crux :: DVar -> EBF -> UnifCrux DVar NTV
  crux o (Left  x)              = CAssign o (NTBase x)
  crux o (Right (f,as))         = CStruct o as f
-}

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
                             DetErroneous -> 0
                             DetFailure   -> 0
                             Det     -> 0
                             DetSemi -> 1
                             DetNon  -> 2 ** (fromIntegral $ length $
                                              filter isFree (o:is))
                                        - 1
                             DetMulti -> 2
    OPIndr _ _          -> 100
    OPEmit _ _ _ _      -> 0

  loops = fromIntegral . length . filter isLoop

  isFree :: ModedVar -> Bool
  isFree v = iIsFree $ nExpose (v^.mv_mi)

  isLoop :: DOpAMine fbs -> Bool
  isLoop = (== DetNon) . detOfDop

------------------------------------------------------------------------}}}
-- Planning                                                             {{{


data PartialPlan fbs = PP { pp_cruxes         :: S.Set (Crux DVar TBase)
                          , pp_binds          :: BindChart
                          , pp_restrictSearch :: Bool
                          , pp_score          :: Cost
                          , pp_plan           :: Actions fbs
                          }

pp_liveVars :: PartialPlan fbs -> S.Set DVar
pp_liveVars p = allCruxVars (pp_cruxes p)

-- XXX This does not have a way to signal UFNotReached back to its caller.
-- That is particularly disappointing since any unification producing that
-- means that there's certainly no plan for the whole rule.
stepPartialPlan :: (Crux DVar TBase -> SIMCT Identity DFunct (Actions fbs))
                -- ^ Possible actions
                -> (PartialPlan fbs -> Actions fbs -> Cost)
                -- ^ Plan scoring function
                -> PartialPlan fbs
                -> Either (Cost, Actions fbs) [PartialPlan fbs]
stepPartialPlan poss score p =
  {- XT.trace ("SPP:\n"
             ++ "  " ++ show (pp_cruxes p) ++ "\n"
             ++ "  " ++ show (pp_binds p) ++ "\n"
           ) $ -}
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
                            (\(act,bc') -> PP rc' bc' r' (score p act) (pl ++ act)
                                           : ps)
                            plan
                ) []

planner_ :: (Crux DVar TBase -> SIMCT Identity DFunct (Actions fbs))
         -- ^ Available steps
         -> (PartialPlan fbs -> Actions fbs -> Cost)
         -- ^ Scoring function
         -> S.Set (Crux DVar TBase)
         -- ^ Cruxes to be planned over
         -> Maybe (EvalCrux DVar, DVar, DVar)
         -- ^ Maybe the updated evaluation crux, the interned
         -- representation of the term being updated, and
         -- result variable.
         -> SIMCtx DVar
         -- ^ Unbound variables in the rule
         -> [(Cost, Actions fbs)]
         -- ^ Plans and their costs
planner_ st sc cr mic ictx = runAgenda
   $ PP { pp_cruxes = cr
        , pp_binds  = ctx'
        , pp_restrictSearch = False
        , pp_score  = 0
        , pp_plan   = ip
        }
 where
  runAgenda = go . (flip mioaPlan M.empty)
   where
    mioaPlan :: PartialPlan fbs
             -> M.Map Cost [PartialPlan fbs]
             -> M.Map Cost [PartialPlan fbs]
    mioaPlan p@(PP{pp_score=psc}) = mapInOrCons psc p

    go pq = maybe [] go' $ mapMinRepView pq
     where
      go' (p, pq') = case stepPartialPlan st sc p of
                       Left df -> df : (go pq')
                       Right ps' -> go (foldr mioaPlan pq' ps')

  ctx' = either (const $ dynacPanicStr "Unable to bind input variable")
                snd
              $ runIdentity
              $ flip runSIMCT ictx
              $ flip runReaderT (UnifParams True False)
                (mapM_ (unifyUnaliasedNV (nHide $ IUniv UShared)) bis)

  -- XREF:INITPLAN
  (ip,bis) = case mic of
              Nothing -> ([],[])
              Just (CCall o is f, hi, ho) -> ( [ OPPeel is hi f
                                                 , OPAsgn o (NTVar ho)]
                                              , o:is)
              Just (CEval o i, hi, ho) -> ( [ OPAsgn i (NTVar hi)
                                              , OPAsgn o (NTVar ho)]
                                            , [o,i] )

-- | Pick the best plan, but stop the planner from going off the rails by
-- considering at most a constant number of plans.
--
-- XXX This is probably not the right idea
bestPlan :: [(Cost, a)] -> Maybe (Cost, a)
bestPlan []    = Nothing
bestPlan plans = Just $ argmin fst (take 1000 plans)

-- | Add the last Emit verb to a string of actions from the planner.
--
-- XXX This is certainly the wrong answer for a number of reasons, not the
-- least of which is that it adds all variables to the identification set,
-- when really we just want the nondeterministic set.
finalizePlan :: Rule -> Actions fbs -> Actions fbs
finalizePlan r d = d ++ [OPEmit (r_head r) (r_result r) (r_index r)
                              $ S.toList $ allCruxVars (r_cruxes r)]

-- | Given a normalized form and, optionally, an initial crux,
--   saturate the graph and get all the plans for doing so.
--
-- XXX This has no idea what to do about non-range-restricted rules.
planUpdate :: BackendPossible fbs
           -> Rule
           -> (PartialPlan fbs -> Actions fbs -> Cost)
           -> S.Set (Crux DVar TBase)                     -- ^ Normal form
           -> (EvalCrux DVar, DVar, DVar)
           -> SIMCtx DVar
           -> Maybe (Cost, Actions fbs)
planUpdate bp r sc anf mi ictx = fmap (second (finalizePlan r)) $
  bestPlan $ planner_ (possible bp r) sc anf (Just mi) ictx

planInitializer :: BackendPossible fbs -> Rule -> Maybe (Cost, Actions fbs)
planInitializer bp r = fmap (second (finalizePlan r)) $
  let cruxes = r_cruxes r in
  bestPlan $ planner_ (possible bp r) simpleCost cruxes Nothing
             (allFreeSIMCtx $ S.toList $ allCruxVars cruxes)

-- | Given a particular crux and the remaining evaluation cruxes in a rule, 
-- find all the \"later\" evaluations which will need special handling and
-- generate the cruxes necessary to prevent double-counting.
--
-- Consider a rule like @a += b(X) * b(Y).@  This desugars into an ANF with
-- two separate evaluations of @b(_)@.  This is problematic, since we will
-- plan each evaluation separately.  (Note that CSE won't help; we really do
-- mean to compute the cross-product in this case, but not double-count the
-- diagonal!)  The workaround here is to /order/ the evaluations, thus why
-- ANF gives a numeric identifier to each evaluation.
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
-- be planning a single stream of instructions for each dfunctar, rather than
-- each evalution arc, but it's not quite clear that there's a nice
-- graphical story to be told in that case?
--
-- XXX What do we do in the CEval case??  We need to check every evaluation
-- inside a CEval update?
handleDoubles :: (Ord a, Ord b)
              => (Int -> a -> a -> a) 
              -> (Int,EvalCrux a)
              -> S.Set (Int, EvalCrux a)
              -> S.Set (UnifCrux a b)
handleDoubles vc e r = S.fold (go e) S.empty r
 where
  go (en, CEval _ ei)      (qn, CEval _ qi)      s =
    if en > qn then s else S.insert (CNotEqu ei qi) s
  go (en, CCall eo eis ef) (qn, CEval qo qi)     s =
    if en > qn then s else let cv = vc 0 eo qo
                            in S.insert (CStruct cv eis ef)
                             $ S.insert (CNotEqu cv qi) s
  go (en, CEval eo ei)     (qn, CCall qo qis qf) s =
    if en > qn then s else let cv = vc 0 eo qo
                            in S.insert (CStruct cv qis qf)
                             $ S.insert (CNotEqu cv ei) s
  go (en, CCall eo eis ef) (qn, CCall qo qis qf) s =
    if en > qn || ef /= qf || length eis /= length qis
     then s
     else let ecv = vc 0 eo qo
              qcv = vc 1 eo qo
           in S.insert (CStruct ecv eis ef)
            $ S.insert (CStruct qcv qis qf)
            $ S.insert (CNotEqu ecv qcv) s

-- XXX Split into two functions, one which wraps handleDoubles and one which
-- feeds that to the planner.  The former will also be useful in dumping
-- more accurate ANF.
planEachEval :: BackendPossible fbs     -- ^ The backend's primitive support
             -> (DFunctAr -> Bool)      -- ^ Indicator for constant function
             -> Rule
             -> [(Int, Maybe (Cost, DVar, DVar, Actions fbs))]
-- planEachEval _ _ _ = []
planEachEval bp cs r  =
  map (\(n,cr) ->
          let
              -- pending eval cruxes
              pecs = (S.delete cr $ S.fromList ecs)

              -- Additional unification cruxes introduced to prevent double
              -- counting
              antidup = S.map Right $ handleDoubles mkvar cr pecs

              -- cruxes to feed to the planner
              cruxes' = S.unions [ S.map Right $ r_ucruxes r
                                 , S.map Left  $ pecs
                                 , antidup
                                 ]

              -- Initialize the context to have variables for all the
              -- variables in cruxes' as well as the crux we're holding out.
              ictx = allFreeSIMCtx
                      $ S.toList
                      $ allCruxVars
                      $ S.insert (Left cr) cruxes'

          in (n, varify $ planUpdate bp r simpleCost cruxes' (mic $ snd cr) ictx))
    -- Filter out non-constant evaluations
    --
    -- XXX This instead should look at the update modes of each evaluation
  $ MA.mapMaybe (\ec -> case ec of
                  (n, CCall _ is f) | not (cs (f,length is))
                                -> Just (n, ec)
                  (_, CCall _ _  _) -> Nothing
                  (n, CEval _ _   ) -> Just (n,ec))

    -- Grab all evaluations
  $ ecs
 where
  mkvar n v1 v2 = B.concat ["chk",v1,"_",v2,"_",BC.pack $ show n]

  ecs = IM.toList $ r_ecruxes r

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
                               [(Rule, Int, Cost, DVar, DVar, Actions fbs)]

-- | Return all plans for each functor/arity
--
-- XXX This may still belong elsewhere.
--
-- XXX This guy wants span information; he's got it now use it.
--
-- timv: might want to fuse these into one circuit
--
combineUpdatePlans :: [(Rule,[( Int,
                                Maybe (Cost, DVar, DVar, Actions fbs))])]
                   -> UpdateEvalMap fbs  
combineUpdatePlans = go (M.empty)
 where
  go m []             = m
  go m ((fr,cmca):xs) = go' xs fr cmca m

  go' xs _  []           m = go m xs
  go' xs fr ((n,mca):ys) m =
    case mca of
      Nothing -> dynacUserErr
                       $ "No update plan for"
                          <+> maybe "indirection"
                                    (\(f,a) -> pretty f <> char '/' <> pretty a)
                                    fa
                          <+> "in rule at"
                          <> line <> indent 2 (prettySpanLoc $ r_span fr)
      Just (c,v1,v2,a) -> go' xs fr ys $ mapInOrCons fa (fr,n,c,v1,v2,a) m
   where
    fa = evalCruxFA ev
    ev = maybe (dynacPanic $ "Eval index without eval crux in rule "
                             <+> (printANF fr))
               id
               (IM.lookup n (r_ecruxes fr))

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
  go' xs fr (Just (c,v,a)) m = go (mapInOrCons (findHeadFA fr)
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
                           CCall _ is f | not $ isMath f
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
