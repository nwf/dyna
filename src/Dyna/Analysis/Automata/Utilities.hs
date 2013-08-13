---------------------------------------------------------------------------
-- | Utilities for automata library

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dyna.Analysis.Automata.Utilities where

import           Control.Arrow (first)
import           Control.Lens
import           Control.Monad.State
import qualified Data.Foldable                   as F
import qualified Data.Traversable                as T
import qualified Data.Map                        as M
import qualified Data.Maybe                      as MA
import           Dyna.Analysis.Automata.Class
import           Dyna.Analysis.Automata.NamedAut
import           Dyna.XXX.PPrint
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Pretty Printing                                                      {{{

autRender :: (T.Traversable f, Automata a)
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

------------------------------------------------------------------------}}}
