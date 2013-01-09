---------------------------------------------------------------------------
-- | What does it mean to be a backend?

-- Header material                                                      {{{
{-# LANGUAGE ExistentialQuantification #-}

module Dyna.Main.BackendDefn where

import qualified Data.Set as S
import           Dyna.Analysis.Aggregation (AggMap)
import           Dyna.Analysis.ANF (Rule)
import           Dyna.Analysis.RuleMode (Action, BackendPossible, Cost,
                                         UpdateEvalMap, QueryEvalMap)
import           Dyna.Term.TTerm (DFunctAr)
import           System.IO (Handle)

-- XXX The notion of be_constants is not quite right, I think?  It is used
-- only in Dyna.Analysis.RuleMode.planEachEval to avoid generating some
-- plans, but that's not really how we should be doing it.  The right
-- answer, of course, is to use update mode information, once we have it.

type BackendDriver bs = AggMap                   -- ^ Aggregation
                      -> UpdateEvalMap bs        -- ^ Rule update
                      -> QueryEvalMap bs         -- ^ Rule query
                      -> [(Rule,Cost,Action bs)] -- ^ Initializers
                      -> Handle                  -- ^ Output
                      -> IO ()

data Backend = forall bs . Backend
             { -- | Builtin support hook for mode planning.  Options are
               --   to return
               -- 
               --   * @Left False@  -- This functor is not built in
               --
               --   * @Left True@   -- There is no plan for this mode
               --
               --   * @Right (d,b)@ -- There is a plan here with determinism
               --                      @d@ and backend-specific data @b@.
               be_builtin :: BackendPossible bs

               -- | Any constants made available by this backend.
             , be_constants :: S.Set DFunctAr
              
               -- | Backend driver
             , be_driver  :: BackendDriver bs
             }
