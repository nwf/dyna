---------------------------------------------------------------------------
-- | The types which constitute the output of the parser

--   Header material                                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.ParserHS.Types (
    -- * Parser output types
    NameWithArgs(..),
    -- ** Surface langauge
    Term(..), Rule(..),
    -- ** Pragmas
    ParsedInst(..), ParsedModeInst, Pragma(..),
    -- ** Lines
    PLine(..),
) where


import qualified Data.ByteString                  as B
import qualified Data.Data                        as D
import           Dyna.Analysis.Mode.Inst
import           Dyna.Main.Defns
import           Dyna.Term.TTerm (Annotation(..), TBase(..),
                                  DFunct)
import           Dyna.Term.SurfaceSyntax
import           Text.Trifecta

------------------------------------------------------------------------}}}
-- Parsed output definitions                                            {{{

data Term = TFunctor B.ByteString
                     [Spanned Term]
          | TAnnot   (Annotation (Spanned Term))
                     (Spanned Term)
          | TVar     B.ByteString
          | TBase    TBase
 deriving (D.Data,D.Typeable,Eq,Ord,Show)

-- | Rules are not just terms because we want to make it very syntactically
--   explicit about the head being a term (though that's not an expressivity
--   concern -- just use the parenthesized texpr case) so that there is no
--   risk of parsing ambiguity.
data Rule = Rule (Spanned Term) B.ByteString (Spanned Term)
 deriving (Eq,Show)

data NameWithArgs = PNWA B.ByteString [B.ByteString]
 deriving (Eq,Show)

-- | Pragmas that are recognized by the parser
data Pragma = PDispos SelfDispos B.ByteString [ArgDispos]
                -- ^ Assert the evaluation disposition of a functor

            | PDisposDefl String
                -- ^ Specify the default disposition handlers
                --   for subsequent context.
                --
                --   Note that the override defintions are
                --   preserved across this operation!
                --   (XXX is that what we want?)

            | PIAggr B.ByteString Int B.ByteString
                -- ^ Assert the aggregator for a functor/arity.

            | PInst NameWithArgs
                    ParsedInst
                -- ^ Declare an instantiation state: name and body

            | PMode NameWithArgs
                    ParsedModeInst
                    ParsedModeInst
                -- ^ Declare a mode: name, input, and output

            | POperAdd Fixity Integer B.ByteString
                -- ^ Add an operator

            | POperDel B.ByteString
                -- ^ Remove an operator
 
            | PRuleIx RuleIx
                -- ^ Set the rule index.
                --
                -- XXX This is a bit of a hack to allow external drivers to
                -- feed rules incrementally; those drivers should treat the
                -- rule index as an opaque token rather than something to be
                -- interpreted.  Eventually this will go away, when our
                -- REPLs have captive compilers.

            {- --- | PMisc Term
                -- ^ Fall-back parser for :- lines. -}
 deriving (Eq,Show)

-- | The type of a parsed inst declaration
data ParsedInst = PIVar   !B.ByteString
                | PIInst  !(InstF DFunct ParsedInst)
 deriving (Eq,Show)

type ParsedModeInst = Either NameWithArgs ParsedInst

data PLine = PLRule (Spanned Rule)
           | PLPragma Pragma
 deriving (Show)

------------------------------------------------------------------------}}}
