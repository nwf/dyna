---------------------------------------------------------------------------
-- | Pick among backends

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Backend.Backends (
  backendHelp, backendMap, parseBackend, noBackend
) where

import           Data.Char
import qualified Data.Map                     as M
import           Dyna.Backend.BackendDefn
import           Dyna.Backend.NoBackend
import           Dyna.Backend.Python.Backend
import           Dyna.Main.Exception
import           Text.PrettyPrint.Free        as PP

------------------------------------------------------------------------}}}
-- Backend Map                                                          {{{

backendMap :: M.Map String (Doc e,Backend)
backendMap = M.fromList
             [ ("none", ("null backend for early stages only", noBackend))
             , ("python", ("generate python code",pythonBackend))
             ]

------------------------------------------------------------------------}}}
-- Backend Parser                                                       {{{

parseBackend :: String -> Backend
parseBackend s = maybe (dynacThrow $ InvocationError
                                   $ "Unknown backend:" <+> pretty s)
                       snd
                       $ M.lookup (map toLower s) backendMap

------------------------------------------------------------------------}}}
-- Backend Help Info                                                    {{{

backendHelp :: Doc e
backendHelp = above "\nBackends available: "
              $ indent 4 $ vcat
                   $ map (\(k,v) -> pretty k <+> colon <+> fst v)
                   $ M.assocs backendMap

------------------------------------------------------------------------}}}

