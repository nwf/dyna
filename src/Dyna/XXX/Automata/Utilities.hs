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
import           Dyna.XXX.Automata.Class
import           Dyna.XXX.Automata.NamedAut
import           Dyna.XXX.PPrint
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Pretty Printing                                                      {{{

-- | Given a ply-by-ply rendering function, render an automaton.  The
-- callback should not inspect or manipulate the @Doc e@ in a ply in order
-- to be a faithful printout of the automaton.
autRender :: (T.Traversable f, AutomataRepr a)
          => (f (Doc e) -> Doc e)
          -> a f -> Doc e
autRender f a =
  let (r, defs) = autReduceIx
                    (\(x :: Int) -> (pan x, M.empty))
                    (\x fr -> let r  = f $ fmap fst fr
                                  ms = M.unions $ F.toList $ fmap snd fr
                              in  (pan x, M.insert x r ms))
                    a
  in r <+> "where" <+> valign (map defrow $ M.toList defs)
 where
  pan = angles . pretty
  defrow (k,v) = pan k <+> equals <+> v

-- XXX Maybe we should consider a version which does not reveal state labels
-- for non-recursive terms.  "<0> where <0> = U@sh" is a lot more verbose
-- than "U@sh", even if more standardized.

------------------------------------------------------------------------}}}
