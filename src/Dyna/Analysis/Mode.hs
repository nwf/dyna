---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system
--
-- For this module and all modules under Dyna.Analysis.Mode, we are
-- incredibly indebted to David Overton and the Mercury Prolog system:
-- David Overton. Precise and Expressive Mode Systems for Typed Logic
-- Programming Languages.  University of Melbourne, Department of Computer
-- Science and Software Engineering.  Ph.D. thesis. December, 2003.
-- <http://www.mercury.csse.unimelb.edu.au/information/papers.html#dmo-thesis>
--
-- The sub-modules have been organized in an effort to be as transparently
-- ascriptive to the thesis as possible.  This module is primarily concerned
-- with execution details which are not part of the thesis exposition,
-- primarily by running computable algorithms over the regular trees of the
-- thesis.

-- Header material                                                      {{{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode(
  module Dyna.Analysis.Mode.Det,
  module Dyna.Analysis.Mode.Uniq,
  module Dyna.Analysis.Mode.Inst,
  module Dyna.Analysis.Mode.Unification,
  module Dyna.Analysis.Mode.Mode,
 
  module Dyna.Analysis.Mode.Execution.NamedInst

  -- module Dyna.Analysis.Mode.InstMap,
  -- module Dyna.Analysis.Mode.Execution.Base,
  -- module Dyna.Analysis.Mode.Execution.Functions,
) where

import           Dyna.Analysis.Mode.Det
import           Dyna.Analysis.Mode.Execution.NamedInst
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq

------------------------------------------------------------------------}}}
