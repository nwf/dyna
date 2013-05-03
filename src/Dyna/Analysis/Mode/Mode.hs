---------------------------------------------------------------------------
-- | Query (and update) modes.

{-# LANGUAGE TemplateHaskell #-}

module Dyna.Analysis.Mode.Mode where

import Control.Lens.TH
import Dyna.Analysis.Mode.Det

-- | A Query Mode is a collection of instantiation transducers.
--
-- This is a surrogate for defn 3.1.10 (p34) which avoids the need to name
-- variables in a query.  Many of the well-formedness conditions fall away,
-- but the subsumption test does not; see 'mWellFormed'
--
-- XXX In the same way that 'InstF' will need generalization when we replace
-- our mode system, so will this.
data QMode n = QMode
             { _qmode_args   :: [(n,n)]
             , _qmode_result :: (n,n)
             , _qmode_det    :: Det
             }
 deriving Show
$(makeLenses ''QMode)

-- XXX Update Modes
