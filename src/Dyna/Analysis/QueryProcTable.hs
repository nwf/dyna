---------------------------------------------------------------------------
-- | Query Procedure Table
--
-- A central point of the compiler, providing answers to ``Can I make this
-- query?''
--
-- XXX Ironically, this code doesn't deal well with full materialization
-- except by a truly terrible hack.
--
-- XREF:PONDERMORE
-- XXX At the moment, we consider only implied modes where the query inst
-- and procedure output inst are both ground.  More generally, we should
-- search for possible unification strategies, rather than "knowing" that
-- OPCheq can handle ground-ground.
--
-- XXX More generally, we should be figuring out how to combine modes in the
-- table to produce others, by case analysis.  Implied modes as in Mercury
-- are just a special case.
--
--   While it would be wrong to have an implied move of a checked downcast
--   to the procedure's input inst, since procedures may be partial in their
--   support, it is correct to have a checked downcast to the type of query
--   of which this procedure is a part.  (For a concrete example, consider
--   +/2 on a generic numeric type; there is a particular procedure which
--   implements a particular information flow of +/2 on integer types, let's
--   say.  If we have a int|float|string type coming in, it'd be wrong to
--   fail the float and string branches and simply return the int answers,
--   despite that there is a procedure with total support on int.  However,
--   since the type of +/2 excludes string, we are justified in checked
--   downcast failing those branches immediately.  Equivalently, the case
--   analysis which splits int|float|string into its three constituents does
--   not mode fail despite that there are no procedures for +/2 on strings.
--
-- XXX The formulation here does not permit us to indicate aliasing across
-- call boundaries.  More generally we should probably be returning SIMCtx
-- transformers rather than output insts?

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.QueryProcTable (
    QueryProcTable,
    QueryProcEntry(..),
    QueryProcQuery(..),
    QueryProcResult(..),
    emptyQueryProcTable,
    addQueryProc,
    buildQueryCodegen,    -- For test harness only
    tryQuery
) where

import qualified Data.ByteString.Char8                   as B8
import qualified Data.IntMap                             as IM
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Term.TTerm
-- import qualified Debug.Trace                             as XT

------------------------------------------------------------------------}}}
-- Definition                                                           {{{

newtype QueryProcId = QPI Int
 deriving (Eq,Ord,Show)

type QPECodeGen d = DVar -> [DVar] -> DOpAMine d

data QueryProcEntry d = QPE { qpe_functor  :: DFunct
                            , qpe_resinsts :: (NIX DFunct, NIX DFunct)
                            , qpe_arginsts :: [(NIX DFunct, NIX DFunct)]
                            , qpe_cg       :: QPECodeGen d
                            }
                        -- XXX This is a rather sad special case just to get
                        -- us something useable
                      | QPEFullMat { qpe_functor :: DFunct
                                   , qpe_fm_argc :: Int
                                   }

instance (Show d) => Show (QueryProcEntry d) where
  show (QPE f ri ai cg) =
       "QPE (" ++ (show f)
    ++ ") ("
    ++ (show ri)
    ++ ") ("
    ++ (show ai)
    ++ ") ("
    ++ (show $ cg "ResultVar" (map (B8.pack.("V"++).show) [1..length ai]))
    ++ ")"
  show (QPEFullMat f n) = "QPEFullMat " ++ (show f) ++ "/" ++ (show n)

newtype QueryProcTable d = QPT (IM.IntMap (QueryProcEntry d))
 deriving (Show)

data QueryProcQuery = QPQ { qpq_functor :: DFunct
                          , qpq_res     :: (DVar, NIX DFunct)
                          , qpq_args    :: [(DVar, NIX DFunct)]
                          }
 deriving (Show)

data QueryProcResult d = QPR { qpr_dopamine :: DOpAMine d
                             , qpr_resinst  :: NIX DFunct
                             , qpr_arginst  :: [NIX DFunct]
                             , qpr_justification :: QueryProcId
                                -- ^ The installed procedure in use in
                                -- the result given, after all the implicit
                                -- machinery has been wrapped around it.
                                --
                                -- XXX This should be a list / set?
                             }
 deriving (Show)

------------------------------------------------------------------------}}}
-- Procedures                                                           {{{

emptyQueryProcTable :: QueryProcTable d
emptyQueryProcTable = QPT IM.empty

-- | Extend the procedure table with another query.  Return a unique
-- identifier for the procedure in question.  This will be returned along
-- with any answers.
--
-- XXX IM.size isn't going to work once we have the ability to retract
-- things.  It is strictly a placeholder!
addQueryProc :: QueryProcTable d
             -> QueryProcEntry d
             -> (QueryProcId, QueryProcTable d)
addQueryProc (QPT t) qpe = let n = IM.size t in (QPI n, QPT (IM.insert n qpe t))

-- | Given a query and an entry in the query table, see what we can make of
--   it.
buildQueryCodegen :: QueryProcQuery
                  -> QueryProcEntry d
                  -> Maybe (DOpAMine d, NIX DFunct, [NIX DFunct])
-- buildQueryCodegen q e | XT.traceShow ("BQC",q,e) False = undefined
buildQueryCodegen (QPQ f _   _   ) qpe | f /= (qpe_functor qpe) = Nothing 

buildQueryCodegen (QPQ _ _   as  ) (QPEFullMat _ n) | n /= length as = Nothing
buildQueryCodegen (QPQ f rin ains) (QPEFullMat _ _) = do
    -- In the case of full materialization, we leave the dispatching to the
    -- backend, so we just run along and make sure that this is something we
    -- expect the backend to be able to handle.  This is such a hack. XXX
    (mvr:mvas) <- mapM mkMV (rin:ains)
    return ( OPIter mvr mvas f (mkDet (snd rin) (map snd ains)) Nothing
           , shared
           , replicate (length ains) shared
           )
 where
   mkMV (v,i) = case () of
                  _ | nFree i     -> Just (MV v i shared)
                  _ | subShared i -> Just (MV v i i)
                  _ | otherwise   -> Nothing

   mkDet _ ais = case () of
                   _ | all subShared ais -> DetSemi
                   _ | otherwise         -> DetNon

   subShared = (`nSub` shared)
   shared    = nHide $ IUniv UShared
    
buildQueryCodegen (QPQ _ rin ains) (QPE _ qim qam qcg) =
  goArgs ains qam (\v vs -> qcg v vs)
 where
  goArgs [] [] gcg = goRes (\rv -> gcg rv [])
  goArgs [] _  _   = Nothing
  goArgs _  [] _   = Nothing
  goArgs ((av,ai):ais) (qis:qiss) gcg = do
    argOp <- ponder paSub paImplg ai qis
    goArgs ais qiss (argOp av gcg)

  -- These functions describe how to mutate the codegen on an
  -- argument-by-argument basis.  Note that the resulting code
  -- generator is expecting one fewer arguments than the input
  -- code generator.
  paSub, paImplg :: DVar
                 -> (DVar -> [DVar] -> DOpAMine d)
                 -> DVar -> [DVar] -> DOpAMine d
  paSub   v cg = \rv avs -> cg rv (v:avs)
  paImplg v cg = \rv avs -> OPScop $ \v' -> OPBloc [ cg rv (v':avs), OPCheq v v' ]

  -- The result gets special handling.
  --
  -- We don't need to pass in the nominal result variable as it's
  -- in scope.
  goRes :: (DVar -> DOpAMine d)
        -> Maybe (DOpAMine d, NIX DFunct, [NIX DFunct])
  goRes acg = do
    resOp <- ponder prSub prImplg (snd rin) qim
    return (resOp acg (fst rin), snd qim , map snd qam )

  -- As above, these describe how to mutate the code gen for implied modes.
  prSub, prImplg :: (DVar -> DOpAMine d) -> (DVar -> DOpAMine d)
  prSub   cg = \rv -> cg rv
  prImplg cg = \rv -> OPScop $ \v' -> OPBloc [ cg v', OPCheq rv v']

  -- Consider a variable for direct or implied mode handling.
  --
  -- XXX XREF:PONDERMORE
  ponder sub implg qi (pin,pout) =
    case () of
      _ | qi `nSub` pin                           -> Just sub
      _ | nFree pin && nGround qi && nGround pout -> Just implg
      _ | otherwise                               -> Nothing

tryQueryE :: QueryProcQuery
          -> QueryProcId
          -> QueryProcEntry d
          -> Maybe (QueryProcResult d)
tryQueryE qpq eid qpe = do
 (dop,ri,ai) <- buildQueryCodegen qpq qpe
 return (QPR dop
             (nLeqGLB (snd $ qpq_res qpq) ri)
             (zipWith (\(_,l) r -> nLeqGLB l r)
                      (qpq_args qpq) ai)
             eid)

-- | Given a query procedure table and a query, return all possible
-- sequences of opcodes that execute that answer that query, together with
-- information about the resulting insts.
tryQuery :: QueryProcTable d
         -> QueryProcQuery
         -> [QueryProcResult d]
tryQuery (QPT t) qpq = IM.elems $ IM.mapMaybeWithKey (tryQueryE qpq . QPI) t

------------------------------------------------------------------------}}}
