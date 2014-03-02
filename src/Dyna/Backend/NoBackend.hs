---------------------------------------------------------------------------
-- | A backend that does no code generation.
--
-- It is anticipated that this will be useful for debugging the earlier
-- stages of the compiler.
--
-- XXX Add a self-test that all primOps modes are supported by other
-- backends.

-- Header material                                                      {{{

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Dyna.Backend.NoBackend (noBackend) where

import           Control.Lens
import           Control.Monad
import qualified Data.Maybe                   as MA
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           Dyna.Analysis.ANF
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.RuleMode (Actions,Cost)
import           Dyna.Backend.BackendDefn
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils
import           Dyna.XXX.PPrint
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free

import qualified Debug.Trace                as XT

------------------------------------------------------------------------}}}
-- Definition                                                           {{{

noBackend :: Backend
noBackend = Backend
          { be_aggregators    = Nothing
          , be_debug_dop_iter = \_ _ _ _ _ -> empty
          , be_driver         = driver
          , be_queryProcs     = []
          , be_updateProcs    = []
          }

driver _ _ _ _ _ _ fh = hPutStrLn fh "No backend selected; stopping."

------------------------------------------------------------------------}}}
