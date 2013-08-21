---------------------------------------------------------------------------
-- | The types which constitute the output of the parser

--   Header material                                                      {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.ParserHS.Types (
    -- * Parser configuration
    DLCfg(..), mkDLCExprParser,
    -- * Parser output types
    NameWithArgs(..),
    -- ** Surface langauge
    TermF(..), Term(..), Rule(..),
    -- ** Pragmas
    ParsedInst(..), ParsedModeInst, Pragma(..), NamedDefDispos(..),
    -- ** Lines
    PLine(..),
) where


import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8                as BU
import           Data.Monoid (Endo(..))
import           Dyna.Analysis.Mode.Inst
import           Dyna.Main.Defns
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.Term.TTerm (Annotation, DFunct, DFunctAr, TBase)
import           Text.Parser.LookAhead
import           Text.Trifecta

------------------------------------------------------------------------}}}
-- Parser Configuration                                                 {{{

type DLCfgPC m = (Monad m, TokenParsing m, DeltaParsing m,
                  LookAheadParsing m)

data DLCfg = DLC
  { dlc_expr_full :: forall m . (DLCfgPC m) => Endo (m (Spanned Term))
    -- ^ The full expression parser

  , dlc_expr_arg  :: forall m . (DLCfgPC m) => Endo (m (Spanned Term))
    -- ^ A restricted expression parser used to parse
    -- arguments to functors.  Notably, this avoids \",\" and
    -- might avoid similar operators (\"for\", \"whenever\")
    -- and some things that we just don't think make sense
    -- there (\"is\", unification operators) even if they'd
    -- be well-formed.
    
  , dlc_expr_list :: forall m . (DLCfgPC m) => Endo (m (Spanned Term))
    -- ^ An even further restricted expression parser, which
    -- leaves out \"|\" at the top level.

  , dlc_aggrs :: forall m . (DLCfgPC m)
              => m ByteString
    -- ^ Aggregator parser

    -- XXX dlc_aggrs for the moment can only return
    -- primitives; we're going to want to make that
    -- extensible at some point.
  }

mkDLCExprParser :: (DLCfgPC m) => OperSpec -> Endo (m (Spanned Term))
mkDLCExprParser = 
  mkExprParser
   (\o x   -> Term (TPrefix  (BU.fromString o) x  ))
   (\o x   -> Term (TPostfix (BU.fromString o) x  ))
   (\o x y -> Term (TInfix   (BU.fromString o) x y))

------------------------------------------------------------------------}}}
-- Parsed output definitions                                            {{{

type Aggregator = ByteString

-- | A ply of a parsed term
data TermF f = TVar     ByteString	-- incl "_"
             | TPrim    TBase
             | TPrefix  ByteString        f
             | TPostfix ByteString        f
             | TInfix   ByteString        f f
             | TFunctor ByteString       [f]
             | TAnnot      (Annotation f) f
 deriving (Eq,Functor,Show)

-- XXX I'd really like to use the more standard Fix and Compose,
-- but no such luck, since Compose lacks a Show instance.
newtype Term = Term { unTerm :: TermF (Spanned Term) }
 deriving (Eq,Show)

-- | Rules are not just terms because we want to make it very syntactically
--   explicit about the head being a term (though that's not an expressivity
--   concern -- just use the parenthesized texpr case) so that there is no
--   risk of parsing ambiguity.
data Rule = Rule (Spanned Term) Aggregator (Spanned Term)
 deriving (Eq,Show)

data NameWithArgs = PNWA ByteString [ByteString]
 deriving (Eq,Show)

-- | Any named default disposition tables we support
data NamedDefDispos = NDDDyna
 deriving (Bounded,Enum,Eq,Show)

-- | Pragmas that are recognized by the parser
data Pragma = PBackchain DFunctAr
                -- ^ A given functor should be planned for ground
                -- backchaining.

            | PDispos DTPosn SelfDispos ByteString [ArgDispos]
                -- ^ Assert the evaluation disposition of a functor

            | PDisposDefl NamedDefDispos
                -- ^ Specify the default disposition handlers
                --   for subsequent context.
                --
                --   Note that the override defintions are
                --   preserved across this operation!
                --   (XXX is that what we want?)

            | PIAggr ByteString Int ByteString
                -- ^ Assert the aggregator for a functor/arity.

            | PInst NameWithArgs
                    ParsedInst
                -- ^ Declare an instantiation state: name and body

            | PMode NameWithArgs
                    ParsedModeInst
                    ParsedModeInst
                -- ^ Declare a mode: name, input, and output

            | POperAdd Fixity Int ByteString
                -- ^ Add an operator

            | POperDel Fixity ByteString
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
data ParsedInst = PIVar   !ByteString
                | PIInst  !(InstF DFunct ParsedInst)
 deriving (Eq,Show)

type ParsedModeInst = Either NameWithArgs ParsedInst

data PLine = PLRule (Spanned Rule)
           | PLPragma Pragma
 deriving (Show)

------------------------------------------------------------------------}}}
