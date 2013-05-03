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
import qualified Text.PrettyPrint.ANSI.Leijen        as PPA

------------------------------------------------------------------------}}}
-- Dyna Compiler Exceptions                                             {{{

-- | When things go badly for us, they go badly in one of these ways,
-- ideally.
data DynacException =
    -- | The user program contains an error
    UserProgramError (PP.Doc TP.Effect)

    -- | Same as 'UserProgramError' but with ANSI documentation
  | UserProgramANSIError PPA.Doc

    -- | Something went wrong when trying to understand arguments
  | InvocationError (PP.Doc TP.Effect)

    -- | We don't implement a feature yet.
  | Sorry            (PP.Doc TP.Effect)

    -- | Something we did not believe possible actually happened
  | Panic            (PP.Doc TP.Effect)
 deriving (DT.Typeable)

deriving instance Show DynacException
instance Exception DynacException

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

-- | Utility function for throwing a document to render
dynacUserErr, dynacSorry, dynacPanic :: PP.Doc TP.Effect -> a
dynacUserErr = throw . UserProgramError
dynacSorry = throw . Sorry
dynacPanic = throw . Panic

-- | Throw a panic string
dynacPanicStr :: String -> a
dynacPanicStr = throw . Panic . PP.text

-- | Throw an ANSI error; this is used inside the parser, primarily, due to
-- trifecta's movement to the ANSI prettyprinter.
dynacUserANSIErr :: PPA.Doc -> a
dynacUserANSIErr = throw . UserProgramANSIError

-- | A type-restricted version of 'throw'
dynacThrow :: DynacException -> a
dynacThrow = throw

------------------------------------------------------------------------}}}
