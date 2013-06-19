---------------------------------------------------------------------------
-- | Query (and update) modes.

{-# LANGUAGE TemplateHaskell #-}

module Dyna.Analysis.Mode.Mode where

import Control.Lens.TH
import Dyna.Analysis.Mode.Det

-- | A Query Mode is a collection of instantiation transducers.
--
-- Note that 'QMode's do not carry the name of the functor with which they
-- are associated; that should be maintained elsewhere and probably passed
-- around as a pair.  (Or maybe we should revisit this decision, XXX)
--
-- This is a surrogate for defn 3.1.10 (p34) which avoids the need to name
-- variables in a query.  Many of the well-formedness conditions fall away,
-- but the subsumption test does not; see 'mWellFormed'
--
-- XXX In the same way that 'InstF' will need generalization when we replace
-- our mode system, so will this.
--
-- XXX This needs to addtionally indicate whether answers are concentrated
-- or diffuse (and possibly diffuse with which aggregator, or aggregators if
-- we want to go nuts -- consider @f(1) += ...@ @f(2) max= ...@ -- these
-- heads do not unify but are subsumed by @f(free>>ground)@.), if they are
-- returned in sorted order (but whose sorted order?), ...
data QMode n = QMode
             { _qmode_args   :: [(n,n)]
             , _qmode_result :: (n,n)
             , _qmode_det    :: Det
             }
 deriving Show
$(makeLenses ''QMode)

unpackModeInputs :: QMode n -> (n, [n])
unpackModeInputs qm = ( fst $ _qmode_result qm
                      , map fst $ _qmode_args qm)

-- XXX Update Modes
