---------------------------------------------------------------------------
-- | Update Procedure Table
--
-- Dual to the Query Procedure Table, this module tracks the update modes
-- that may be seen in the system.
--
-- XXX For the moment, it's really more of a simplistic placeholder.  And
-- it's kind of confused, at that: there are two, related but different,
-- things we might care about that both belong here:
-- 
--   * Whether or not a given /type/ can accept any updates at all
--
--   * Whether a given /inst/ of a type that does accept an update is
--     supported by an actual procedure.
--
--   * What the update handler for a specific inst may be.

-- Header material                                                      {{{
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.UpdateProcTable {- (
) -} where

import qualified Data.Map                                as M
import           Dyna.Term.TTerm

------------------------------------------------------------------------}}}
-- Definition                                                           {{{

data UpdateProcStatus d = UPSAccepted -- XXX ([DVar] -> DOpAMine d)?
                          -- ^ Is reactive (XXX equipped with the update
                          -- handler code to run)
                        | UPSForbidden
                          -- ^ Is constant.  We mean this in the sense that
                          -- no update will be directed to this item.  That
                          -- does not imply that its value could be known at
                          -- compile time.  (Though for a pure Dyna program,
                          -- without primops, that could be true.)
 deriving (Eq,Ord,Show)

data UpdateProcEntry d = UPE { upe_functar :: DFunctAr
                             , upe_status  :: UpdateProcStatus d
                             }

newtype UpdateProcTable d = UPT (M.Map DFunctAr (UpdateProcStatus d))

------------------------------------------------------------------------}}}
-- Procedures                                                           {{{

emptyUpdateProcTable :: UpdateProcTable d
emptyUpdateProcTable = UPT M.empty

addUpdateProc :: UpdateProcTable d
              -> UpdateProcEntry d
              -> UpdateProcTable d
addUpdateProc (UPT t) (UPE fa s) = UPT (M.insert fa s t)

lookupUpdateProc :: UpdateProcTable d
                 -> DFunctAr
                 -> Maybe (UpdateProcEntry d)
lookupUpdateProc (UPT t) fa = UPE fa `fmap` M.lookup fa t

------------------------------------------------------------------------}}}
