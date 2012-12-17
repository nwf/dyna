---------------------------------------------------------------------------
-- | Top Level Exceptions from the Dyna Compiler
--
-- Modeled on GHC's GhcException

-- Header material                                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dyna.Main.Exception where

import           Control.Exception
import qualified Data.Typeable                       as DT
import qualified System.Console.Terminfo.PrettyPrint as TP
import qualified Text.PrettyPrint.Free               as PP

------------------------------------------------------------------------}}}
-- Dyna Compiler Exceptions                                             {{{

data DynacException =
    -- | The user program contains an error
    UserProgramError (PP.Doc TP.Effect)

    -- | We don't implement a feature yet.
  | Sorry            (PP.Doc TP.Effect)

    -- | Something we did not believe possible actually happened
  | Panic            (PP.Doc TP.Effect)
 deriving (DT.Typeable)

deriving instance Show DynacException
instance Exception DynacException

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

dynacThrow :: DynacException -> a
dynacThrow = throw

dynacSorry = throw . Sorry

------------------------------------------------------------------------}}}
