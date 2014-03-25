---------------------------------------------------------------------------
-- | Utilities for automata library

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.XXX.Automata.Utilities where

import           Control.Arrow (first)
import           Control.Lens
import           Control.Monad.State
import qualified Data.Foldable                   as F
import qualified Data.Traversable                as T
import qualified Data.Map                        as M
import qualified Data.Maybe                      as MA
import           Dyna.XXX.Automata.ReprClass
import           Dyna.XXX.Automata.NamedAut

------------------------------------------------------------------------}}}
