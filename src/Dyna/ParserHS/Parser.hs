---------------------------------------------------------------------------
-- | A parser for some chunk of the Dyna language, using Trifecta
--
-- Based in part on
-- <https://github.com/ekmett/trifecta/blob/master/examples/RFC2616.hs>
-- as well as the trifecta code itself
--
-- Note that, due to @TemplateHaskell@ that this file is not necessarily in
-- the most human-readable order.
--
-- TODO (XXX):
--
--   * We might want to use T.T.Literate, too, in the end.
--
--   * Doesn't understand dynabase literals (\"{ ... }\")
--
--   * Doesn't handle parenthesized aggregators
--
--   * Doesn't handle shared subgoals (\"for ... { ... }\")
--     (Fixing that probably means changing our idea of 'Rule';
--      also revisit XREF:ANFRESERVED if doing so.)
--
--   * Doesn't understand nullary star for gensym correctly
--      (it's a available in term context but not texpr context;
--      this depends on an upstream fix in Text.Parser.Expression.
--      But: I am not worried about it since we don't handle gensyms
--      anywhere else in the pipeline yet)
--
--   Header material                                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.ParserHS.Parser (
    -- * Parser configuration inputs
    EOT, mkEOT, DLCfg(..),
    -- * Parser output types
    NameWithArgs(..),
    -- ** Surface langauge
    Term(..), Rule(..),
    -- ** Pragmas
    ParsedInst(..), ParsedModeInst, Pragma(..),
    -- ** Line
    Line(..),
    -- * Action
    parse,
    -- * Test harness hooks
    testTerm, testAggr, testRule, testPragma,
) where

import           Control.Applicative
import           Control.Monad
-- import           Control.Monad.Identity
import           Control.Monad.Reader
-- import           Control.Monad.State
-- import           Control.Monad.Trans.Either
import qualified Data.ByteString.UTF8             as BU
import qualified Data.ByteString                  as B
import qualified Data.CharSet                     as CS
import qualified Data.Data                        as D
import qualified Data.HashSet                     as H
import qualified Data.Map                         as M
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Main.Defns
import           Dyna.Term.TTerm (Annotation(..), TBase(..),
                                  DFunct, DFunctAr)
import           Dyna.Term.SurfaceSyntax
import           Dyna.XXX.DataUtils
import           Dyna.XXX.Trifecta (identNL,
                                    stringLiteralSQ,unSpan)
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
-- import qualified Text.PrettyPrint.Free            as PP
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
 
            | PQMode DFunctAr 
                -- ^ A query mode declaration

            | PRuleIx RuleIx
                -- ^ Set the rule index.
                --
                -- XXX This is a bit of a hack to allow external drivers to
                -- feed rules incrementally; those drivers should treat the
                -- rule index as an opaque token rather than something to be
                -- interpreted.  Eventually this will go away, when our
                -- REPLs have captive compilers.
            
            | PMisc Term
                -- ^ Fall-back parser for :- lines.
 deriving (Eq,Show)


-- | The type of a parsed inst declaration
data ParsedInst = PIVar   !B.ByteString
                | PIInst  !(InstF DFunct ParsedInst)
 deriving (Eq,Show)

type ParsedModeInst = Either NameWithArgs ParsedInst

data Line = LRule (Spanned Rule)
          | LPragma Pragma
 deriving (Show)

------------------------------------------------------------------------}}}
-- Parser input definitions                                             {{{

-- | Existentialized operator table; this is a bit of a hack, but it will
-- do just fine for now, I hope.
--
-- XXX
newtype EOT = EOT { unEOT :: forall m .
                             (DeltaParsing m, LookAheadParsing m)
                          => OperatorTable m (Spanned Term)
                  }


-- XXX Add support for Haskell-style `foo`.  This requires augmenting
-- the PFIn branch of interpret below to check for the ` framing and
-- change the symbol returned (but not the symbol matched!)
mkEOT :: OperSpec
      -> Bool   -- ^ add some measure of fail-safety using generic
                -- parsers
      -> {- Either (PP.Doc e) -} EOT
mkEOT s0 f0 = EOT $ addFailSafe $ interpSpec M.empty $ M.toList s0
 where
  interpSpec m [] = map snd $ M.toDescList m
  interpSpec m ((o,lfs):os) = interpSpec (foldr go m lfs) os
   where
    go (p,f) = mapInOrCons p (interpret f o)

  interpret (PFIn a) = flip Infix a . bf . spanned . bsf . symbol
  interpret PFPre    = Prefix       . uf . spanned . bsf . symbol
  interpret PFPost   = Postfix      . uf . spanned . bsf . symbol

  addFailSafe = if f0 then (++ failSafe) else id

  failSafe = [ [ Prefix $ uf (spanned $ prefixOper )         ]
             , [ Infix  (bf (spanned $ normOper )) AssocNone ]
             , [ Infix  (bf (spanned $ dotOper  )) AssocNone ]
             ]

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

bsf :: Functor f => f String -> f B.ByteString
bsf = fmap BU.fromString

parseNameWithArgs :: (Monad m, TokenParsing m)
                  => m B.ByteString -> m NameWithArgs
parseNameWithArgs n = PNWA <$> n
                           <*> choice [ parens ( var `sepBy` comma )
                                      , pure []
                                      ]

------------------------------------------------------------------------}}}
-- Parser Monad                                                         {{{

data DLCfg = DLC { dlc_opertab :: EOT }

newtype DynaLanguage m a = DL { unDL :: ReaderT DLCfg m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,
            Parsing,CharParsing,LookAheadParsing)

instance MonadTrans DynaLanguage where
  lift = DL . lift

instance (TokenParsing m, MonadPlus m) => TokenParsing (DynaLanguage m) where
  someSpace = buildSomeSpaceParser (lift someSpace) dynaCommentStyle
  semi      = lift semi
  highlight h (DL m) = DL (highlight h m)

instance DeltaParsing m => DeltaParsing (DynaLanguage m) where
  line = lift line
  position = lift position
  slicedWith f (DL m) = DL $ slicedWith f m
  rend = lift rend
  restOfLine = lift restOfLine

instance (Monad m) => MonadReader DLCfg (DynaLanguage m) where
  ask       = DL ask
  local f m = DL (local f (unDL m))

{-
instance MonadState s m => MonadState s (DynaLanguage m) where
  get = lift get
  put = lift . put
  state = lift . state
-}

------------------------------------------------------------------------}}}
-- Comment handling                                                     {{{

dynaCommentStyle :: CommentStyle
dynaCommentStyle =  CommentStyle
  { _commentStart = "{%" -- XXX?
  , _commentEnd   = "%}" -- XXX?
  , _commentLine  = "%"
  , _commentNesting = True
  }

------------------------------------------------------------------------}}}
-- Identifier Syles                                                     {{{

-- | The full laundry list of punctuation symbols we "usually" mean.
usualpunct :: CS.CharSet
usualpunct = CS.fromList "!#$%&*+/<=>?@\\^|-~:.,"

-- | Dot or comma operators
--
-- Note that these are only safe to use in combination with 'thenAny'.
dynaDotOperStyle :: TokenParsing m => IdentifierStyle m
dynaDotOperStyle = IdentifierStyle
  { _styleName = "Dot-Operator"
  , _styleStart   = char '.'
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

{-
-- | Comma operators
dynaCommaOperStyle :: TokenParsing m => IdentifierStyle m
dynaCommaOperStyle = IdentifierStyle
  { _styleName = "Comma-Operator"
  , _styleStart   = char ','
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }
-}

-- | Prefix operators
--
-- Dot is handled specially elsewhere due to its
-- dual purpose as an operator and rule separator.
--
-- Colon is not a permitted beginning to a prefix
-- operator, as it is a sigil for type annotations.
dynaPfxOperStyle :: TokenParsing m => IdentifierStyle m
dynaPfxOperStyle = IdentifierStyle
  { _styleName = "Prefix Operator"
  , _styleStart   = oneOfSet $ usualpunct CS.\\ CS.fromList ".:,"
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | Infix operators
--
-- Dot is handled specially elsewhere due to its
-- dual purpose as an operator and rule separator.
-- Comma similarly has special handling due to its
-- nature as term and subgoal separator.
dynaOperStyle :: TokenParsing m => IdentifierStyle m
dynaOperStyle = IdentifierStyle
  { _styleName = "Infix Operator"
  , _styleStart   = oneOfSet $ usualpunct CS.\\ CS.fromList ".,"
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

dynaAggNameStyle :: TokenParsing m => IdentifierStyle m
dynaAggNameStyle = IdentifierStyle
  { _styleName = "Aggregator Name"
  , _styleStart   = lower
  , _styleLetter  = letter
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | Aggregators must end with one of these symbols; used to prevent
-- an over-zealous interpretation of concatenation as a rule.
aggTermSyms :: CS.CharSet
aggTermSyms = CS.fromList "=-"

dynaNameStyle :: TokenParsing m => IdentifierStyle m
dynaNameStyle = IdentifierStyle
  { _styleName = "Name"
  , _styleStart    = (lower <|> oneOf "$")
  , _styleLetter   = (alphaNum <|> oneOf "_'")
  , _styleReserved = H.fromList [ "for", "is", "new", "whenever" ] -- XXX maybe not?
  , _styleHighlight = Constant
  , _styleReservedHighlight = ReservedOperator
  }

name :: (Monad m, TokenParsing m) => m B.ByteString
name = bsf $ ident dynaNameStyle

dynaVarStyle :: TokenParsing m => IdentifierStyle m
dynaVarStyle = IdentifierStyle
  { _styleName = "Variable"
  , _styleStart    = (upper <|> char '_')
  , _styleLetter   = (alphaNum <|> oneOf "_'")
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

var :: (Monad m, TokenParsing m) => m B.ByteString
var = bsf $ ident dynaVarStyle

------------------------------------------------------------------------}}}
-- Atoms                                                                {{{

parseAtom :: (Monad m, TokenParsing m) => m B.ByteString
parseAtom = (liftA BU.fromString stringLiteralSQ <|> name) <?> "Atom"

parseFunctor :: (Monad m, TokenParsing m) => m B.ByteString
parseFunctor = highlight Identifier parseAtom <?> "Functor"

------------------------------------------------------------------------}}}
-- Terms and term expressions                                           {{{

nullaryStar :: DeltaParsing m => m (Spanned Term)
nullaryStar = spanned $ flip TFunctor [] <$> (bsf $ string "*")
                      <* (notFollowedBy $ char '(')

term :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
     => m (Spanned Term)
term = token $ choice
        [       parens tfexpr
        ,       spanned $ TVar <$> var

        ,       spanned $ mkta <$> (colon *> term) <* whiteSpace <*> term

        , try $ spanned $ TBase . TString  <$> bsf stringLiteral

        , try $ spanned $ TBase . TNumeric <$> naturalOrDouble

        , try $ spanned $ flip TFunctor [] <$> parseAtom
                        <* (notFollowedBy $ char '(')

        , try $ nullaryStar
        ,       spanned $ parenfunc
        ]
 where
  parenfunc = TFunctor <$> parseFunctor
                       <*>  parens (tlexpr `sepBy` symbolic ',')

  mkta ty te = TAnnot (AnnType ty) te

-- | Sometimes we require that a character not be followed by whitespace
-- and satisfy some additional predicate before we pass it off to some other parser.
thenAny :: (Monad m, TokenParsing m, LookAheadParsing m)
        => m a -> m Char
thenAny p =    anyChar                             -- some character
            <* lookAhead (notFollowedBy someSpace) -- not followed by space
            <* lookAhead p                         -- and not follwed by the request

-- | A "dot operator" is a dot followed immediately by something that looks
-- like a typical operator.  We 'lookAhead' here to avoid the case of a dot
-- by itself as being counted as an operator; the dot operator is required
-- to have not-a-space following (to avoid confusion with the end-of-rule
-- marker, which is taken to be "dot space" or "dot eof").
dotOper :: (Monad m, TokenParsing m, LookAheadParsing m)
        => m B.ByteString
dotOper = bsf $ try (lookAhead (thenAny anyChar) *> identNL dynaDotOperStyle)

-- XXX Temporarily eliminated because of confusion with "foo(a,&b)" -- we
-- need to punt this out of the general expression table and down into the
-- "full" table (or perhaps something in-between?) -- it should be OK to
-- write "f(a, (b ,, c))" if ",," is an infix operator, for example, but
-- maybe "f(a, b  ,, c )" is a syntax error.
{-
-- | A "comma operator" is a comma necessarily followed by something that
-- would continue to be an operator (i.e. punctuation).
commaOper :: (Monad m, TokenParsing m, LookAheadParsing m)
          => m B.ByteString
commaOper = bsf $ try (   lookAhead (thenAny $ _styleLetter dynaCommaOperStyle)
                       *> identNL dynaCommaOperStyle)
                       -}

-- | A normal operator is handled by trifecta's built-in handling
normOper :: (Monad m, TokenParsing m) => m B.ByteString
normOper = bsf $ ident dynaOperStyle

-- | Prefix operators also handled by trifecta's built-in handling
prefixOper :: (Monad m, TokenParsing m) => m B.ByteString
prefixOper = bsf $ ident dynaPfxOperStyle

uf :: (Monad m, Applicative m)
   => m (Spanned B.ByteString)
   -> m (Spanned Term -> Spanned Term)
uf f = do
  (x:~spx)  <- f
  pure (\a@(_:~sp)   -> (TFunctor x [a]):~(spx <> sp))

bf :: (Monad m, Applicative m)
   => m (Spanned B.ByteString)
   -> m (Spanned Term -> Spanned Term -> Spanned Term)
bf f = do
  (x:~spx)  <- f
  pure (\a@(_:~spa) b@(_:~spb) -> (TFunctor x [a,b]):~(spa <> spx <> spb))


tlexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m (Spanned Term)
tlexpr = asks dlc_opertab >>= flip buildExpressionParser term . unEOT

moreETable :: (LookAheadParsing m, DeltaParsing m) => [[Operator m (Spanned Term)]]
moreETable = [ [ Infix  (bf (spanned $ bsf $ symbol "is"      )) AssocNone  ]
             , [ Infix  (bf (spanned $ bsf $ symbol ","       )) AssocRight ]
             -- , [ Infix  (bf (spanned $       commaOper        )) AssocRight ]
             , [ Infix  (bf (spanned $ bsf $ symbol "whenever")) AssocNone
               , Infix  (bf (spanned $ bsf $ symbol "for"     )) AssocNone  ]
             ]

-- | Full Expression
tfexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m (Spanned Term)
tfexpr = buildExpressionParser moreETable tlexpr <?> "Expression"

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

-- XXX There must be a better way.
parseAggr :: (DeltaParsing m, LookAheadParsing m) => m B.ByteString
parseAggr = token
 (do
   an <- optional (identNL dynaAggNameStyle)
   as <- manyTill (oneOfSet usualpunct)
                  (try $ lookAhead $ oneOfSet aggTermSyms
                                   <* notFollowedBy (oneOfSet usualpunct))
   ae <- oneOfSet aggTermSyms
   bsf (pure $ maybe id (++) an $ as ++ [ae])
 ) <?> "Aggregator"

rule :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
     => m Rule
rule = do
  _ <- optional whiteSpace
  h@(_ :~ hs) <- term
  choice [ do
            _    <- try (char '.' <* lookAhead whiteSpace)
            return (Rule h "|=" (TFunctor "true" [] :~ hs))
         , do
            aggr <- parseAggr
            body <- tfexpr
            _    <- char '.'
            return (Rule h aggr body)
         ]

------------------------------------------------------------------------}}}
-- Pragmas                                                              {{{

-- Inst Declarations                                                    {{{

instDeclNameStyle :: TokenParsing m => IdentifierStyle m
instDeclNameStyle = dynaNameStyle
                    { _styleName = "Inst name"
                    , _styleReserved = H.fromList $ [ "any"
                                                    , "bound"
                                                    , "clobbered"
                                                    , "mostlyclobbered"
                                                    , "free"
                                                    , "shared"
                                                    , "unique"
                                                    , "mostlyunique"
                                                    ]
                    }

instName :: (Monad m, TokenParsing m) => m B.ByteString
instName = bsf $ ident instDeclNameStyle

parseInst :: (Monad m, TokenParsing m) => m ParsedInst
parseInst = choice [ PIVar <$> var
                   , symbol "free"   *> pure (PIInst IFree)
                   , symbol "any"    *> (PIInst . IAny  <$> optUniq)
                   , symbol "ground" *> (PIInst . IUniv <$> optUniq)
                   , symbol "bound"  *> boundinst UShared

                   -- Some uniques are acceptable in this context and have
                   -- slightly different meanings
                   , symbol "unique" *> choice [ boundinst UUnique
                                               , pure (PIInst (IUniv UUnique))
                                               ]
                   , symbol "clobbered" *> pure (PIInst (IUniv UClobbered))
                   ]
 where
  optUniq = parens ( parseUniq ) <|> pure UShared

  -- XXX this $base thing is pretty bad.  Suggestions are welcome.
  boundinst u = braces $ (PIInst <$>) $
     flip (IBound u) <$> choice [ try (symbol "$base" *> optional semi) *> pure True
                                , pure False
                                ]
                     <*> (M.fromList <$> functinst `sepBy` semi )

  functinst = (,) <$> parseAtom <*> parens (parseInst `sepBy` comma)

parseUniq :: (TokenParsing m) => m Uniq
parseUniq = choice [ symbol "clobbered" *> pure UClobbered
                   , symbol "mostlyclobbered" *> pure UMostlyClobbered
                   , symbol "mostlyunique" *> pure UMostlyUnique
                   , symbol "shared" *> pure UShared
                   , symbol "unique" *> pure UUnique
                   ]

------------------------------------------------------------------------}}}

pragmaBody :: (MonadReader DLCfg m, DeltaParsing m, LookAheadParsing m)
           => m Pragma
pragmaBody = choice
  [ -- try $ symbol "aggr" *> parseAggr          -- XXX alternate syntax for aggr
    symbol "dispos" *> parseDisposition -- in-place dispositions
  , symbol "dispos_def" *> parseDisposDefl -- set default dispositions
  , symbol "inst"   *> parseInstDecl    -- instance delcarations
  , symbol "mode"   *> parseMode        -- mode/qmode decls
  , symbol "oper"   *> parseOper        -- new {pre,in,post}fix oper
  , symbol "ruleix" *> (PRuleIx <$> decimal)
  ]
 where
  parseDisposition = PDispos <$> selfdis
                             <*> parseFunctor
                             <*> (parens (argdis `sepBy` comma)
                                  <|> pure [])
   where
    argdis  = choice [ symbol "&" *> pure ADQuote
                     , symbol "*" *> pure ADEval
                     ]
    selfdis = choice [ symbol "&" *> pure SDQuote
                     , symbol "*" *> pure SDEval
                     , pure SDInherit
                     ]

  parseDisposDefl = PDisposDefl <$>
    choice [ symbol "prologish"
           , symbol "dyna"
           , pure "dyna"
           ]

  -- XXX Does not handle <= or >= forms yet, which we need for mode
  -- polymorphism.
  --
  parseInstDecl = PInst <$> parseNameWithArgs instName
                        <*  symbol "=="
                        <*> parseInst

  parseOper = choice [ try $ symbol "add" *> parseOperAdd
                     , try $ symbol "del" *> parseOperDel
                     , parseOperAdd
                     ]

    where
      parseOperAdd = do
               (fx,n) <- fixity
               prec   <- natural
               sym    <- n
               return $ POperAdd fx prec sym

      parseOperDel = POperDel <$> afx
                         
      fixity = choice [ symbol "pre"  *> pure (PFPre, pfx)
                      , symbol "post" *> pure (PFPost, pfx)
                      , symbol "in" *> ((,) <$> (PFIn <$> assoc) <*> pure ifx)
                      ]

      pfx = choice [ prefixOper, dotOper, {- commaOper, -} name ]
      ifx = choice [ normOper  , dotOper, {- commaOper, -} name ]
      afx = choice [ prefixOper, normOper, dotOper, {- commaOper, -} name]

      assoc = choice [ symbol "none"  *> pure AssocNone
                     , symbol "left"  *> pure AssocLeft
                     , symbol "right" *> pure AssocRight
                     ]

  -- Unlike Mercury, mode declarations are used solely to give names to
  -- modes.  We separate query modes and update modes out to their own
  -- pragmas, qmode and umode.
  parseMode = PMode <$> parseNameWithArgs name
                    <*  symbol "=="
                    <*> (Right <$> parseInst <|> Left <$> parseNameWithArgs instName)
                    <*  symbol ">>"
                    <*> (Right <$> parseInst <|> Left <$> parseNameWithArgs instName)


pragmaline :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
           => m Pragma
pragmaline =    symbol ":-"
             *> whiteSpace
             *> (pragmaBody
                  <|> fmap PMisc (unSpan <$> tfexpr <?> "Other pragma"))
             <* whiteSpace
             <* {- optional -} (char '.')


------------------------------------------------------------------------}}}
-- Lines                                                                {{{

dline :: (MonadReader DLCfg m, DeltaParsing m, LookAheadParsing m)
      => m (Spanned Line)
dline = whiteSpace
        *> spanned (choice [ LPragma <$> pragmaline
                           , LRule <$> spanned rule
                           ])

configureParser :: (DeltaParsing m, LookAheadParsing m)
                => DynaLanguage m a
                -> DLCfg
                -> m a
configureParser p c = runReaderT (unDL p) c

-- | The grand Dyna parser.
parse :: (DeltaParsing m, LookAheadParsing m) => DLCfg -> m (Spanned Line)
parse = configureParser dline

------------------------------------------------------------------------}}}
-- Test hooks                                                           {{{

testTerm   :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m (Spanned Term)
testTerm   = configureParser term

testAggr   :: (DeltaParsing m, LookAheadParsing m)
           => m B.ByteString
testAggr   = parseAggr

testRule   :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m Rule
testRule   = configureParser rule

testPragma :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m Pragma
testPragma = configureParser pragmaBody

------------------------------------------------------------------------}}}
