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
--   * The decoupling of DTPosn and DFunctAr is wrong (it accepts nonsense);
--     we should probably use DTPosn as a functor.

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.ParserHS.Parser (
    -- * Parser configuration inputs
    DLCfg(..), mkDLC,
    dynaWhiteSpace, genericAggregators,

    -- * Action!
    parse,

    -- * Test harness hooks
    testTerm, testRule, testPragma,
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
import qualified Data.HashSet                     as H
import qualified Data.Map                         as M
import           Data.Monoid (Endo(..), mempty)
import           Data.Semigroup ((<>))
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Uniq
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.ParserHS.Syntax
import           Dyna.ParserHS.Types
import           Dyna.Term.TTerm (Annotation(..), TBase(..))
import           Dyna.XXX.Trifecta (identNL)
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

bsf :: Functor f => f String -> f B.ByteString
bsf = fmap BU.fromString

-- csa :: Spanned (a -> b) -> a -> Spanned b
-- csa (f :~ s) a = (f a) :~ s

expr :: (MonadReader DLCfg m, DeltaParsing m, LookAheadParsing m)
     => (DLCfg -> Endo (m (Spanned Term))) -> m (Spanned Term)
expr ep = join (asks ((`appEndo` term) . ep))

expr_full, expr_arg, expr_list
  :: (MonadReader DLCfg m, DeltaParsing m, LookAheadParsing m)
  => m (Spanned Term)
expr_full = (try (expr dlc_expr_full) <|> term) <?> "Expression"
expr_arg  = (try (expr dlc_expr_arg ) <|> term) <?> "Argument"
expr_list = (try (expr dlc_expr_list) <|> term) <?> "List element"

parseNameWithArgs :: (Monad m, TokenParsing m)
                  => m B.ByteString -> m NameWithArgs
parseNameWithArgs n = PNWA <$> n
                           <*> choice [ parens ( var `sepBy` comma )
                                      , pure []
                                      ]

tn :: B.ByteString -> Term
tn f = Term $ TFunctor f []

tb :: B.ByteString -> Spanned Term -> Spanned Term -> Term
tb f a b = Term $ TFunctor f [a,b]

------------------------------------------------------------------------}}}
-- Building Dyna Language Configurations                                {{{

mkDLC :: OperSpec -> DLCfg
mkDLC os = DLC
  { dlc_expr_full = mkDLCExprParser os
  , dlc_expr_arg  = mkDLCExprParser os_arg
  , dlc_expr_list = mkDLCExprParser os_list
  , dlc_aggrs     = genericAggregators          -- XXX
  }
 where
  os_arg = mkArgOS os
  os_list = mkListOS os

------------------------------------------------------------------------}}}
-- Parser Monad                                                         {{{

newtype DynaLanguage m a = DL { unDL :: ReaderT DLCfg m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,
            Parsing,CharParsing,LookAheadParsing)

instance MonadTrans DynaLanguage where
  lift = DL . lift

instance (TokenParsing m, MonadPlus m) => TokenParsing (DynaLanguage m) where
  someSpace = dynaWhiteSpace (lift someSpace)
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

dynaWhiteSpace :: (TokenParsing m) => m () -> m ()
dynaWhiteSpace m = buildSomeSpaceParser m dynaCommentStyle

------------------------------------------------------------------------}}}
-- Identifier Syles                                                     {{{

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
parseAtom = (stringLiteral' <|> name) <?> "Atom"

parseFunctor :: (Monad m, TokenParsing m) => m B.ByteString
parseFunctor = highlight Identifier parseAtom <?> "Functor"

------------------------------------------------------------------------}}}
-- Terms and term expressions                                           {{{

nullaryStar :: DeltaParsing m => m (Spanned Term)
nullaryStar = spanned $ (string "*" *> pure (Term $ TFunctor "*" []))

term :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
     => m (Spanned Term)
term = token $ choice
        [       parens expr_full
        ,       st TVar var

        -- XXX ,       spanned $ mkta <$> (colon *> term) <*> term

        , try $ st (TPrim . TString) (bsf stringLiteral)

        , try $ st (TPrim . TNumeric) naturalOrDouble

        , try $ st (TPrim . TBool) boolean

        , try $ nullaryStar

        ,       nakedbrak
        ,       parenfunc
        ]
        <?> "Term"
 where
  st a p = spanned $ Term . a <$> p

  mkta ty te = TAnnot (AnnType ty) te

  boolean = choice [ symbol "true" *> return True
                   , symbol "false" *> return False
                   ]

  parenfunc = do
    f :~ sf <- spanned parseFunctor
    mas <- optional $ spanned (parens (expr_arg `sepBy` symbolic ','))
    let as :~ sas = maybe ([] :~ sf) id mas
        sp = sf <> sas
    return $ Term (TFunctor f as) :~ sp

  nakedbrak = listify <$> tlist
   where
    tlist = spanned $
      brackets ((,) <$> (expr_list `sepEndBy` symbolic ',')
                    <*> (optional (symbolic '|' *> expr_arg))
               )
    listify ((xs,ml) :~ s) =
      let (xs' :~ s') = foldr (\a@(_ :~ sa) b@(_ :~ sb) ->
                                 (tb dynaConsOper a b) :~ (sa <> sb))
                              (maybe (tn dynaNilOper :~ r s) id ml)
                              xs
      in (xs' :~ (s <> s'))
    r (Span _ e b) = Span e e b

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

-- XXX There must be a better way.
genericAggregators :: (DeltaParsing m, LookAheadParsing m) => m B.ByteString
genericAggregators = token
 (do
   an <- optional (identNL dynaAggNameStyle)
   as <- manyTill (oneOfSet usualpunct)
                  (try $ lookAhead $ oneOfSet aggTermSyms
                                   <* notFollowedBy (oneOfSet usualpunct))
   ae <- oneOfSet aggTermSyms
   bsf (pure $ maybe id (++) an $ as ++ [ae])
 ) <?> "Aggregator"

rule :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
     => m (Spanned Rule)
rule = token $ do
  h@(_ :~ hs) <- term
  choice [ do
            (_ :~ ds) <- try (spanned (char '.') <* lookAhead whiteSpace)
            return (Rule h ":-" (Term (TPrim (TBool True)) :~ ds)
                     :~ (hs <> ds))
         , do
            aggr    <- token $ join $ asks dlc_aggrs
            body    <- expr_full
            _ :~ ds <- spanned (char '.')
            return (Rule h aggr body :~ (hs <> ds))
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
                   , symbol "bound"  *> (optBUniq >>= boundinst)

                   -- Some uniques are acceptable in this context and have
                   -- slightly different meanings
                   , symbol "unique" *> choice [ boundinst UUnique
                                               , pure (PIInst (IUniv UUnique))
                                               ]
                   , symbol "clobbered" *> pure (PIInst (IUniv UClobbered))
                   ]
 where
  optUniq  = parens   ( parseUniq ) <|> pure UShared
  optBUniq = brackets ( parseUniq ) <|> pure UShared

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
-- Parsing pragma bodies                                                {{{

pragmaBody :: forall m .
              (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
           => m Pragma
pragmaBody = token $ choice
  [
    symbol "backchain" *> parseBackchain
  , symbol "dispos_def" *> parseDisposDefl -- set default dispositions
  , symbol "dispos" *> parseDisposition -- in-place dispositions
  , symbol "iaggr"  *> parseIAggr       -- alternate syntax for aggr
  , symbol "inst"   *> parseInstDecl    -- instance delcarations
  , symbol "mode"   *> parseMode        -- mode/qmode decls
  , symbol "oper"   *> parseOper        -- new {pre,in,post}fix oper
  , symbol "ruleix" *> (PRuleIx <$> decimal)
  ]
 where
  parseArity :: m Int
  parseArity = do
    n <- token decimal
    when (n > fromIntegral (maxBound :: Int)) $ unexpected "huge number"
    return (fromIntegral n)

  parseBackchain = PBackchain <$> (   (,)
                                   <$> parseFunctor
                                   <*  char '/'
                                   <*> parseArity)

  parseDisposition = PDispos <$> posn
                             <*> selfdis
                             <*> parseFunctor
                             <*> (parens (argdis `sepBy` comma)
                                  <|> pure [])
   where
    posn    = choice [ symbol "pre"   *> pure DTPPrefix
                     , symbol "post"  *> pure DTPPostfix
                     , symbol "in"    *> pure DTPInfix
                     , symbol "funct" *> pure DTPFunctor
                     , pure DTPFunctor
                     ]

    argdis  = choice [ symbol "&" *> pure ADQuote
                     , symbol "*" *> pure ADEval
                     ]
    selfdis = choice [ symbol "&" *> pure SDQuote
                     , symbol "*" *> pure SDEval
                     , pure SDInherit
                     ]

  parseDisposDefl = PDisposDefl <$>
    choice [ symbol "dyna" *> pure NDDDyna
           -- , symbol "prologish"
           , pure NDDDyna
           ]

  parseIAggr = do
    f <- parseFunctor
    _ <- char '/'
    n <- parseArity
    a <- join $ asks dlc_aggrs
    return (PIAggr f (fromIntegral n) a)

  -- XXX Does not handle <= or >= forms yet, which we need for mode
  -- polymorphism.
  --
  parseInstDecl = PInst <$> parseNameWithArgs instName
                        <*  symbol "=="
                        <*> parseInst

  parseOper = choice [ symbol "add" *> parseOperAdd
                     , symbol "del" *> parseOperDel
                     , parseOperAdd
                     ]

    where
      parseOperAdd = do
               (fx,n) <- fixity
               prec   <- natural
               when (prec > fromIntegral (maxBound :: Int))
                    $ unexpected "huge number"
               sym    <- n
               return $ POperAdd fx (fromIntegral prec) sym

      parseOperDel = do
                      (fx,n) <- fixity
                      sym <- n
                      return $ POperDel fx sym

      fixity = choice [ symbol "pre"  *> pure (PFPre, pfx)
                      , symbol "post" *> pure (PFPost, pfx)
                      , symbol "in" *> ((,) <$> (PFIn <$> assoc) <*> pure ifx)
                      ]

      pfx = choice [ bsf $ prefixOper, bsf $ dotOper, {- commaOper, -} name ]
      ifx = choice [ bsf $ infixOper , bsf $ dotOper, {- commaOper, -} name ]

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

------------------------------------------------------------------------}}}

pragma :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m Pragma
pragma = token (symbol ":-" *> pragmaBody <* {- optional -} (char '.'))

------------------------------------------------------------------------}}}
-- Lines                                                                {{{

dline :: (MonadReader DLCfg m, DeltaParsing m, LookAheadParsing m)
      => m (Spanned PLine)
dline = spanned (choice [ PLPragma <$> pragma
                        , PLRule <$> rule
                        ])

configureParser :: (DeltaParsing m, LookAheadParsing m)
                => DynaLanguage m a
                -> DLCfg
                -> m a
configureParser p c = runReaderT (unDL p) c

-- | The grand Dyna parser.
parse :: (DeltaParsing m, LookAheadParsing m) => DLCfg -> m (Spanned PLine)
parse = configureParser dline

------------------------------------------------------------------------}}}
-- Test hooks                                                           {{{

testTerm   :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m (Spanned Term)
testTerm   = configureParser term

testRule   :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m (Spanned Rule)
testRule   = configureParser rule

testPragma :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m Pragma
testPragma = configureParser pragma

------------------------------------------------------------------------}}}
