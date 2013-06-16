---------------------------------------------------------------------------
-- | Functions for pretty-printing Insts
--
-- Intended to be imported qualified
--
-- XXX Maybe some of the parser's handling of insts should end up over here,
-- too?

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}
module Dyna.Analysis.Mode.InstPretty where

import qualified Data.Map                as M
import           Data.String
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Text.PrettyPrint.Free

compactFreeFlag :: (IsString a) => Bool -> a
compactFreeFlag True  = "a"
compactFreeFlag False = "u"

compactUniq :: (IsString a) => Uniq -> a
compactUniq UUnique          = "un"
compactUniq UMostlyUnique    = "mu"
compactUniq UShared          = "sh"
compactUniq UMostlyClobbered = "mc"
compactUniq UClobbered       = "cl"

fullUniq :: (IsString a) => Uniq -> a
fullUniq UUnique          = "unique"
fullUniq UMostlyUnique    = "mostlyunique"
fullUniq UShared          = "shared"
fullUniq UMostlyClobbered = "mostlyclobbered"
fullUniq UClobbered       = "clobbered"
            
compactly :: (fr -> Doc e)
          -> (f -> Doc e)
          -> (a -> Doc e)
          -> InstF fr f a -> Doc e
compactly q _ _ (IFree f)       = "F" <> (q f)
compactly _ _ _ (IAny u)        = "A@" <> compactUniq u
compactly _ _ _ (IUniv u)       = "U@" <> compactUniq u
compactly _ f a (IBound u bm b) = (semiBraces $ if b then (text "B"):rm else rm)
                                <> char '@' <> compactUniq u
    where
     rm = map (\(k,vs) -> f k <> tupled (map a vs)) (M.toList bm)
