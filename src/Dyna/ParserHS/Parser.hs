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
    EOT, mkEOT, DLCfg(..),
    dynaWhiteSpace, genericAggregators,
    -- ** Pragmas
    renderPragma,
    -- * Action
    parse,
    -- * Test harness hooks
    testTerm, testGenericAggr, testRule, testPragma,
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
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Dyna.Analysis.Mode.Inst
import qualified Dyna.Analysis.Mode.InstPretty    as IP
import           Dyna.Analysis.Mode.Uniq
import           Dyna.ParserHS.Types
import           Dyna.Term.TTerm (Annotation(..), TBase(..),
                                  DFunct)
import           Dyna.Term.SurfaceSyntax
import           Dyna.XXX.DataUtils
import           Dyna.XXX.Trifecta (identNL,
                                    stringLiteralSQ)
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import qualified Text.PrettyPrint.Free            as PP
import           Text.Trifecta

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
--
-- XXX On parser failure, we get a huge mass of cruft for "expected: ...",
-- since it blats out the entire operator table.  Can we fix that?
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

  interpret (PFIn a) = flip Infix a . bf . interpCore
  interpret PFPre    = Prefix       . uf . interpCore
  interpret PFPost   = Postfix      . uf . interpCore

  -- Make operators use the longest match
  interpCore s = try (spanned (bsf (symbol s))
                      <* notFollowedBy (oneOfSet usualpunct))

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

data DLCfg = DLC { dlc_opertab :: EOT
                 , dlc_aggrs   :: forall m .
                                  (CharParsing m, DeltaParsing m,
                                   LookAheadParsing m)
                               => m B.ByteString
                 }

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

parseAtom :: (Monad m, TokenParsing m) => m DFunct
parseAtom = (liftA BU.fromString stringLiteralSQ <|> name) <?> "Atom"

parseFunctor :: (Monad m, TokenParsing m) => m DFunct
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

        ,       spanned $ mkta <$> (colon *> term) <*> term

        , try $ spanned $ TBase . TString  <$> bsf stringLiteral

        , try $ spanned $ TBase . TNumeric <$> naturalOrDouble

        , try $ spanned $ flip TFunctor [] <$> parseAtom
                        <* (notFollowedBy $ char '(')

        , try $ nullaryStar
        ,       nakedbrak
        ,       spanned $ parenfunc
        ]
        <?> "Term"
 where
  mkta ty te = TAnnot (AnnType ty) te

  parenfunc = TFunctor <$> parseFunctor
                       <*> parens (tlexpr `sepBy` symbolic ',')

  nakedbrak = listify <$> tlist
   where
    tlist = spanned (brackets ((,) <$> (tlistexpr `sepEndBy` symbolic ',')
                                       <*> (optional (symbolic '|' *> tlexpr))
                                  ))

    listify ((xs,ml) :~ s) =
      let (xs' :~ s') = foldr (\a@(_ :~ sa) b@(_ :~ sb) ->
                                TFunctor "cons" [a,b] :~ (sa <> sb))
                              (maybe (TFunctor "nil" [] :~ r s) id ml)
                              xs
      in (xs' :~ (s <> s'))
    r (Span _ e b) = Span e e b


  -- XXX Ick ick ick ick... there must be a more general answer, even if
  -- involves patching ekmett-parsers to understand something more like
  -- DOPP.
  --
  -- XREF:TLEXPR
  tlistexpr = do
    ot <- asks dlc_opertab
    (buildExpressionParser (maskPipe (unEOT ot)) term) <?> "List Expression"
   where
    maskPipe ot = fmap (fmap mkpf) ot

    mkpf (Infix m a) = Infix (nfp >> m) a
    mkpf (Prefix m)  = Prefix (nfp >> m)
    mkpf (Postfix m)  = Postfix (nfp >> m)

  nfp = notFollowedBy (symbolic '|' *> notFollowedBy (oneOfSet usualpunct))

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

-- XREF:TLEXPR
tlexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m (Spanned Term)
tlexpr = (asks dlc_opertab >>= flip buildExpressionParser term . unEOT)
         <?> "Core Expression"

moreETable :: (LookAheadParsing m, DeltaParsing m) => [[Operator m (Spanned Term)]]
moreETable = [ [ Infix  (bf (spanned $ bsf $ symbol dynaEvalAssignOper)) AssocNone  ]
             , [ Infix  (bf (spanned $ bsf $ symbol dynaConjOper      )) AssocRight ]
             -- , [ Infix  (bf (spanned $       commaOper        )) AssocRight ]
             , map (\x -> Infix  (bf (spanned $ bsf $ symbol x)) AssocNone)
                   dynaRevConjOpers
             ]

-- | Full Expression
tfexpr :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m (Spanned Term)
tfexpr = buildExpressionParser moreETable tlexpr <?> "Expression"

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
            return (Rule h "|=" (TFunctor "true" [] :~ ds) :~ (hs <> ds))
         , do
            aggr    <- token $ join $ asks dlc_aggrs
            body    <- tfexpr
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

------------------------------------------------------------------------}}}
-- Printing pragma bodies                                               {{{

renderFunctor :: B.ByteString -> PP.Doc e
renderFunctor f = PP.squotes (PP.pretty f)

renderInst :: ParsedInst -> PP.Doc e
renderInst (PIVar v)               = PP.pretty v
renderInst (PIInst IFree)          = "free"
renderInst (PIInst (IAny u))       = "any" PP.<> PP.parens (IP.fullUniq u)
renderInst (PIInst (IUniv u))      = "ground" PP.<> PP.parens (IP.fullUniq u)
renderInst (PIInst (IBound u m b)) =
  "bound" PP.<> PP.brackets (IP.fullUniq u)
          PP.<> (if b then "$base" PP.<> PP.semi else PP.empty)
          PP.<> (PP.encloseSep PP.lparen PP.rparen PP.semi
                 $ map (\(k,v) -> PP.pretty k PP.<> PP.tupled (map renderInst v))
                 $ M.toList m)

renderMode :: ParsedModeInst -> PP.Doc e
renderMode = either renderPNWA renderInst

renderPNWA :: NameWithArgs -> PP.Doc e
renderPNWA (PNWA n as) = PP.pretty n PP.<> PP.tupled (map PP.pretty as)

renderPragma_ :: Pragma -> PP.Doc e
renderPragma_ (PBackchain (f,a)) = "backchain" PP.<+> renderFunctor f
                                               PP.<> PP.char '/'
                                               PP.<> PP.pretty a

renderPragma_ (PDisposDefl s) = "dispos_def" PP.<+> PP.text s

renderPragma_ (PDispos s f as) = "dispos" PP.<+> rs s
                                          PP.<> renderFunctor f
                                          PP.<> PP.tupled (map ra as)
 where
  rs SDInherit = PP.empty
  rs SDQuote   = "&"
  rs SDEval    = "*"

  ra ADQuote   = "&"
  ra ADEval    = "*"

renderPragma_ (PIAggr f a ag)  = "iaggr" PP.<+> renderFunctor f
                                         PP.<> PP.char '/'
                                         PP.<> PP.pretty a
                                         PP.<+> PP.pretty ag
                                         PP.<+> PP.empty

renderPragma_ (PInst n i) = "inst" PP.<+> renderPNWA n
                                   PP.<+> renderInst i

renderPragma_ (POperAdd f i n) = "oper" PP.<+> "add"
                                        PP.<+> rf f
                                        PP.<+> PP.pretty i
                                        PP.<+> PP.pretty n
 where
  rf PFPre  = "pre"
  rf PFPost = "post"
  rf (PFIn a) = "in" PP.<+> ra a

  ra AssocLeft  = "left"
  ra AssocNone  = "none"
  ra AssocRight = "right"

renderPragma_ (POperDel n) = "oper" PP.<+> "del" PP.<+> PP.pretty n

renderPragma_ (PMode n i o) = "mode" PP.<+> renderPNWA n
                                     PP.<+> renderMode i
                                     PP.<+> renderMode o

renderPragma_ (PRuleIx r) = "ruleix" PP.<+> PP.pretty r

renderPragma :: Pragma -> PP.Doc e
renderPragma = PP.enclose ":-" PP.dot . renderPragma_

------------------------------------------------------------------------}}}

pragma :: (DeltaParsing m, LookAheadParsing m, MonadReader DLCfg m)
       => m Pragma
pragma = token $
     symbol ":-"
  *> (pragmaBody
      -- <|> fmap PMisc (unSpan <$> tfexpr <?> "Other pragma")
     )
  <* {- optional -} (char '.')


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

testGenericAggr :: (DeltaParsing m, LookAheadParsing m)
                => m B.ByteString
testGenericAggr = genericAggregators

testRule   :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m (Spanned Rule)
testRule   = configureParser rule

testPragma :: (DeltaParsing m, LookAheadParsing m)
           => DLCfg -> m Pragma
testPragma = configureParser pragma

------------------------------------------------------------------------}}}
