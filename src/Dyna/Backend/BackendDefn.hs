---------------------------------------------------------------------------
-- | What does it mean to be a backend?

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Dyna.Backend.BackendDefn where

import qualified Data.Set                         as S
import           Dyna.Analysis.Aggregation (AggMap)
import           Dyna.Analysis.ANF (Rule)
import           Dyna.Analysis.DOpAMine (BackendRenderDopIter)
import           Dyna.Analysis.RuleMode (Actions, Cost)
import           Dyna.Analysis.QueryProcTable (QueryProcEntry)
import           Dyna.Analysis.UpdateProcTable (UpdateProcEntry)
import           Dyna.Term.TTerm (DFunctAr,DVar)
import           System.IO (Handle)
import qualified Text.PrettyPrint.Free            as PP

------------------------------------------------------------------------}}}
-- Define a backend                                                     {{{

-- XXX The notion of be_constants is not quite right, I think?  It is used
-- only in Dyna.Analysis.RuleMode.planEachEval to avoid generating some
-- plans, but that's not really how we should be doing it.  The right
-- answer, of course, is to use update mode information, once we have it.

type BackendDriver bs = AggMap                       -- ^ Aggregation
                      -> [(Rule,[(Int,Maybe DFunctAr
                                 , Cost, DVar, DVar, Actions bs)])] -- ^ Rule update
                      -> [(Rule,Cost,Actions bs)] -- ^ Initializers
                      -> S.Set DFunctAr           -- ^ Ground backchains
                      -> [(DFunctAr,Rule,([DVar],(Cost,Actions bs)))] -- ^ GBC plans
                      -> (forall e . PP.Doc e)    -- ^ Parser persistence
                      -> Handle                   -- ^ Output
                      -> IO ()

data Backend = forall bs . Backend
             { -- | Aggregators exported by this backend, if specified.
               --
               -- If not given, the parser will use a generic set of
               -- aggregators.
               --
               -- XXX This is not really right, as the set of aggregators is
               -- a property of Dyna, not of the backend, but for the
               -- moment...
               be_aggregators :: Maybe (S.Set String)

               -- | Debugging hook to render bits of DOpAMine which
               -- are "backend-specific"
             , be_debug_dop_iter :: forall e . BackendRenderDopIter bs e

               -- | Backend driver
             , be_driver  :: BackendDriver bs

               -- | Any additional procedures we know about and would like
               -- to register.  The actual act of registration takes place
               -- by the user of the backend; the procedure identifiers are
               -- not of interest to the backend.
             , be_queryProcs :: [QueryProcEntry bs]

               -- | Any update procedures provided by the backend.
               --
               -- Note that this can include "forbidden" update procedures,
               -- to indicate that 
             , be_updateProcs :: [UpdateProcEntry bs]
             }

------------------------------------------------------------------------}}}
