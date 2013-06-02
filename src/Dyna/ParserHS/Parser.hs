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
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.ParserHS.Parser (
    PCS, defPCS,
    Term(..), rawDTerm,
    Rule(..), RuleIx, rawDRule, rawDRules, Line(..), rawDLine, rawDLines
) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
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
import           Dyna.Main.Exception
import           Dyna.Term.TTerm (Annotation(..), TBase(..),
                                  DFunct, DFunctAr, DVar)
import           Dyna.Term.SurfaceSyntax
import           Dyna.XXX.MonadUtils (incState)
import           Dyna.XXX.Trifecta (identNL,prettySpanLoc,
                                    stringLiteralSQ,unSpan)
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.Free    as PP
import           Text.Trifecta

------------------------------------------------------------------------}}}
-- Parsed output definition                                             {{{

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
--
--   Each 'Rule' additionally carries its own 'DisposTab' for conversion to
--   ANF.  We cannot return just one 'DisposTab' when we are done parsing
--   because each 
data Rule = Rule RuleIx (Spanned Term) B.ByteString (Spanned Term)
                 DisposTab

instance Show Rule where
 showsPrec p (Rule i h a b _) = showParen (p > 9) $
   showString "Rule " .
   showsPrec 6 i .
   showString " " .
   showsPrec 6 h .
   showString " " .
   showsPrec 6 a .
   showString " " .
   showsPrec 6 b .
   showString " _"

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

            | POperAdd PragmaFixity Integer B.ByteString
                -- ^ Add an operator

            | POperDel B.ByteString
                -- ^ Remove an operator
 
            | PQMode DFunctAr 
                -- ^ A query mode declaration
            
            | PMisc Term
                -- ^ Fall-back parser for :- lines.
 deriving (Eq,Show)

data PragmaFixity = PFIn PAssoc | PFPre | PFPost
 deriving (Eq,Show)

-- XXX This is only necessary until parsers upstream cuts a release in which
-- 'Assoc' is 'Eq' and 'Show'.  It's already committed upstream, but...
data PAssoc = PAssocNone | PAssocLeft | PAssocRight
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
-- Parser Configuration State                                           {{{

-- | Existentialized operator table; this is a bit of a hack, but it will
-- do just fine for now, I hope.
--
-- XXX
newtype EOT = EOT { unEOT :: forall m .
                             (DeltaParsing m, LookAheadParsing m)
                          => OperatorTable m (Spanned Term)
                  }

-- | Configuration state threaded into the parser
--
-- Note that this type is hidden with the exception of some accessors below.
data PCS =
  PCS { _pcs_dt_mk     :: DisposTabOver -> DisposTab
      , _pcs_dt_over   :: DisposTabOver
      , _pcs_instmap   :: M.Map B.ByteString ([DVar]
                                             ,ParsedInst
                                             ,Span)
        -- ^ Collects inst pragmas
        --
        -- XXX add arity to key?
      , _pcs_modemap   :: M.Map B.ByteString ([DVar]
                                             ,ParsedModeInst
                                             ,ParsedModeInst
                                             ,Span)
        -- ^ Collects mode pragmas
        --
        -- XXX add arity to key?
      , _pcs_opertab   :: EOT
      , _pcs_operspec  :: M.Map B.ByteString () -- XXX
      , _pcs_ruleix    :: RuleIx
      }
$(makeLenses ''PCS)

pcs_dt = liftA2 ($) (use pcs_dt_mk) (use pcs_dt_over)

newtype PCM im a = PCM { unPCM :: StateT PCS im a }
 deriving (Alternative,Applicative,CharParsing,DeltaParsing,
           Functor,LookAheadParsing,Monad,MonadPlus,Parsing,TokenParsing)

instance (Monad im) => MonadState PCS (PCM im) where
  get = PCM get
  put = PCM . put
  state = PCM . state

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

bsf :: Functor f => f String -> f B.ByteString
bsf = fmap BU.fromString

parseNameWithArgs n = PNWA <$> n
                           <*> choice [ parens ( var `sepBy` comma )
                                      , pure []
                                      ]

-- | Smart constructor for building a rule with index
rule :: (Functor f, MonadState PCS f)
     => f (   Spanned Term
           -> B.ByteString
           -> Spanned Term
           -> DisposTab
           -> Rule)
rule = Rule <$> (pcs_ruleix <<%= (+1))

rs :: (MonadState a m) => ReaderT a m b -> m b
rs x = get >>= runReaderT x

defPCS = PCS { _pcs_dt_mk     = disposTab_dyna
             , _pcs_dt_over   = mempty
             , _pcs_instmap   = mempty -- XXX
             , _pcs_modemap   = mempty -- XXX
             , _pcs_operspec  = mempty -- XXX
             , _pcs_opertab   = EOT $
                -- The basic expression table for limited expressions.
                --
                -- Notably, this excludes @,@ (which is important
                -- syntactically), @for@, @whenever@, and @is@ (which are
                -- nonsensical in local context)
                -- XXX right now all binops are at equal precedence and
                -- left-associative; that's wrong.
                --
                -- XXX timv suggests that this should be assocnone for
                -- binops as a quick fix.  Eventually we should still do
                -- this properly.
                --
                -- XXX this ought to be derived from the default
                -- _pcs_operspec rather than being coded as it is.
                [ [ Prefix $ uf (spanned $ bsf $ symbol "new") ]
                , [ Prefix $ uf (spanned $ prefixOper )             ]
                , [ Infix  (bf (spanned $ normOper )) AssocLeft  ]
                , [ Infix  (bf (spanned $ dotOper  )) AssocRight ]
                ]
             , _pcs_ruleix    = 0
             }

------------------------------------------------------------------------}}}
-- Comment handling                                                     {{{

dynaCommentStyle :: CommentStyle
dynaCommentStyle =  CommentStyle
  { _commentStart = "{%" -- XXX?
  , _commentEnd   = "%}" -- XXX?
  , _commentLine  = "%"
  , _commentNesting = True
  }

newtype DynaLanguage m a = DL { unDL :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,
            Parsing,CharParsing,LookAheadParsing)

instance MonadTrans DynaLanguage where
  lift = DL

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

instance MonadState s m => MonadState s (DynaLanguage m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (DynaLanguage m) where
  ask = lift ask
  local f m = DL $ local f (unDL m)

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

dynaAggStyle :: TokenParsing m => IdentifierStyle m
dynaAggStyle = IdentifierStyle
  { _styleName = "Aggregator"
  , _styleStart   =     (oneOfSet $ usualpunct CS.\\ CS.fromList ".,")
                    <|> lower
  , _styleLetter  =     (oneOfSet $ usualpunct)
                    <|> alphaNum
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | Aggregators must end with one of these symbols; used to prevent
-- an over-zealous interpretation of concatenation as a rule.
aggTermSyms :: H.HashSet Char
aggTermSyms = H.fromList "=-"

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

parseFunctor = highlight Identifier parseAtom <?> "Functor"

------------------------------------------------------------------------}}}
-- Terms and term expressions                                           {{{

nullaryStar :: DeltaParsing m => m (Spanned Term)
nullaryStar = spanned $ flip TFunctor [] <$> (bsf $ string "*")
                      <* (notFollowedBy $ char '(')

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
normOper = bsf $ ident dynaOperStyle

-- | Prefix operators also handled by trifecta's built-in handling
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


tlexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader PCS m)
       => m (Spanned Term)
tlexpr = view pcs_opertab >>= flip buildExpressionParser term . unEOT

moreETable :: (LookAheadParsing m, DeltaParsing m) => [[Operator m (Spanned Term)]]
moreETable = [ [ Infix  (bf (spanned $ bsf $ symbol "is"      )) AssocNone  ]
             , [ Infix  (bf (spanned $ bsf $ symbol ","       )) AssocRight ]
             -- , [ Infix  (bf (spanned $       commaOper        )) AssocRight ]
             , [ Infix  (bf (spanned $ bsf $ symbol "whenever")) AssocNone
               , Infix  (bf (spanned $ bsf $ symbol "for"     )) AssocNone  ]
             ]

-- | Full Expression
tfexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader PCS m)
       => m (Spanned Term)
tfexpr = buildExpressionParser moreETable tlexpr <?> "Expression"

rawDTerm :: (DeltaParsing m, LookAheadParsing m) => m (Spanned Term)
rawDTerm = runReaderT (unDL term) defPCS

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

parseAggr :: (DeltaParsing m) => m B.ByteString
parseAggr =
 (do
   a <- ident dynaAggStyle
   when (not $ (last a) `H.member` aggTermSyms) $
     unexpected "Improper terminal character in aggregator"
   bsf (pure a)
 ) <?> "Aggregator"

parseRule :: (DeltaParsing m, LookAheadParsing m, MonadState PCS m)
          => m Rule
parseRule = optional whiteSpace
          *> choice [
               -- HEAD AGGR TFEXPR .
               try $ rule <*> rs term
                          <*  whiteSpace
                          <*> parseAggr
                          <*> rs tfexpr
                          <*> pcs_dt

               -- HEAD .
             , do
                  h@(_ :~ s) <- rs term
                  rule <*> pure h
                       <*> pure "&="
                       <*> pure (TFunctor "true" [] :~ s)
                       <*> pcs_dt
             ]
         <* {- optional -} (char '.')

rawDRule :: (DeltaParsing m, LookAheadParsing m) => m (Spanned Rule)
rawDRule = evalStateT (unPCM $ unDL $ spanned parseRule) defPCS

rawDRules :: (DeltaParsing m, LookAheadParsing m) => m [Spanned Rule]
rawDRules = evalStateT (unPCM $ unDL $ many (spanned parseRule <* optional whiteSpace)) defPCS

------------------------------------------------------------------------}}}
-- Pragmas                                                              {{{

-- Inst Declarations                                                    {{{

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

instName = bsf $ ident instDeclNameStyle

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

parseUniq = choice [ symbol "clobbered" *> pure UClobbered
                   , symbol "mostlyclobbered" *> pure UMostlyClobbered
                   , symbol "mostlyunique" *> pure UMostlyUnique
                   , symbol "shared" *> pure UShared
                   , symbol "unique" *> pure UUnique
                   ]

------------------------------------------------------------------------}}}

parsePragma = choice
  [ -- try $ symbol "aggr" *> parseAggr          -- XXX alternate syntax for aggr
    symbol "dispos" *> parseDisposition -- in-place dispositions
  , symbol "inst"   *> parseInstDecl    -- instance delcarations
  , symbol "mode"   *> parseMode        -- mode/qmode decls
  , symbol "oper"   *> parseOper        -- new {pre,in,post}fix oper
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

      assoc = choice [ symbol "none"  *> pure PAssocNone
                     , symbol "left"  *> pure PAssocLeft
                     , symbol "right" *> pure PAssocRight
                     ]

  -- Unlike Mercury, mode declarations are used solely to give names to
  -- modes.  We separate query modes and update modes out to their own
  -- pragmas, qmode and umode.
  parseMode = PMode <$> parseNameWithArgs name
                    <*  symbol "=="
                    <*> (Right <$> parseInst <|> Left <$> parseNameWithArgs instName)
                    <*  symbol ">>"
                    <*> (Right <$> parseInst <|> Left <$> parseNameWithArgs instName)


dpragma :: (DeltaParsing m, LookAheadParsing m, MonadReader PCS m)
        => m Pragma
dpragma =    symbol ":-"
          *> whiteSpace
          *> (parsePragma
               <|> fmap PMisc (unSpan <$> tfexpr <?> "Other pragma"))
          <* whiteSpace
          <* {- optional -} (char '.')

pcsProcPragma :: (Parsing m, MonadState PCS m) => Spanned Pragma -> m ()
pcsProcPragma (PDispos s f as :~ _) = do
  pcs_dt_over %= dtoMerge (f,length as) (s,as)
pcsProcPragma (PDisposDefl n :~ s) = do
  pcs_dt_mk .= case n of
                 "dyna" -> disposTab_dyna
                 "prologish" -> disposTab_dyna
                 _ -> dynacPanic $ "Unknown default disposition table:"
                                   PP.<//> PP.pretty n
                                   PP.<//> "at" PP.<//> prettySpanLoc s
pcsProcPragma (PInst (PNWA n as) pi :~ s) = do
  im <- use pcs_instmap
  maybe (pcs_instmap %= M.insert n (as,pi,s))
        -- XXX fix this error message once the new trifecta lands upstream
        -- with its ability to throw Err.
        (\(_,_,s') -> unexpected $ "duplicate definition of inst: "
                                      ++ (show n)
                                      ++ "(prior definition at "
                                      ++ (show s') ++ ")" )
      $ M.lookup n im
pcsProcPragma (PMode (PNWA n as) pmf pmt :~ s) = do
  mm <- use pcs_modemap
  maybe (pcs_modemap %= M.insert n (as,pmf,pmt,s))
        -- XXX fix this error message once the new trifecta lands upstream
        -- with its ability to throw Err.
        (\(_,_,_,s') -> unexpected $ "duplicate definition of mode: "
                                      ++ (show n)
                                      ++ "(prior definition at "
                                      ++ (show s') ++ ")" )
      $ M.lookup n mm
pcsProcPragma (p :~ s) = dynacSorry $ "Cannot handle pragma"
                                      PP.<//> (PP.text $ show p)
                                      PP.<//> "at"
                                      PP.<//> prettySpanLoc s


------------------------------------------------------------------------}}}
-- Lines                                                                {{{

progline :: (MonadState PCS m, DeltaParsing m, LookAheadParsing m)
         => m (Spanned Line)
progline  =    whiteSpace
            *> spanned (choice [ LPragma <$> rs dpragma
                               , LRule <$> spanned parseRule
                               ])

rawDLine :: (DeltaParsing m, LookAheadParsing m) => m (Spanned Line)
rawDLine = evalStateT (unPCM $ unDL $ progline <* optional whiteSpace) defPCS

-- XXX REWRITE

interpretProgline = do
  ls@(l :~ s) <- progline
  case l of
    LPragma  p -> pcsProcPragma (p :~ s) >> interpretProgline
    _ -> return ls

dparse = (unPCM $ unDL $ many (interpretProgline <* optional whiteSpace) <* eof)

rawDLines = evalStateT dparse defPCS

------------------------------------------------------------------------}}}
