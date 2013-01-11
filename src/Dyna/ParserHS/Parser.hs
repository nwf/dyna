---------------------------------------------------------------------------
-- | A parser for some chunk of the Dyna language, using Trifecta
--
-- Based in part on
-- <https://github.com/ekmett/trifecta/blob/master/examples/RFC2616.hs>
-- as well as the trifecta code itself
--
-- TODO (XXX):
--
--   * There is certainly too much special handling of the comma operator,
--     but see COMMAOP below for why it's not so easy to fix.
--
--   * We might want to use T.T.Literate, too, in the end.
--
--   * Doesn't understand dynabase literals ("{ ... }")
--
--   * Doesn't handle parenthesized aggregators
--
--   * Doesn't handle shared subgoals ("whenever ... { ... }")
--
--   * Doesn't understand nullary star for gensym correctly
--      (it's a available in term context but not texpr context;
--      this depends on an upstream fix in Text.Parser.Expression.
--      But: I am not worried about it since we don't handle gensyms
--      anywhere else in the pipeline yet)

--   Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Dyna.ParserHS.Parser (
    Term(..), dterm, -- dtlexpr, dtfexpr,
    Rule(..), drule, Line(..), dline, dlines
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.UTF8             as BU
import qualified Data.ByteString                  as B
import           Data.Char (isSpace)
import qualified Data.CharSet                     as CS
import qualified Data.HashSet                     as H
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Text.Parser.Expression
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta

import           Dyna.Term.TTerm (Annotation(..))
import           Dyna.XXX.MonadUtils (incState)
import           Dyna.XXX.Trifecta (identNL,stringLiteralSQ)

------------------------------------------------------------------------}}}
-- Parsed output definition                                             {{{

data Term = TFunctor !B.ByteString
                     ![Spanned Term]
          | TAnnot   !(Annotation (Spanned Term))
                     !(Spanned Term)
          | TNumeric !(Either Integer Double)
          | TString  !B.ByteString
          | TVar     !B.ByteString
 deriving (Eq,Ord,Show)

type RuleIx = Int

-- | Rules are not just terms because we want to make it very syntactically
--   explicit about the head being a term (though that's not an expressivity
--   concern -- just use the parenthesized texpr case) so that there is no
--   risk of parsing ambiguity.
data Rule = Rule !RuleIx !(Spanned Term) !B.ByteString !(Spanned Term)
 deriving (Eq,Show)

-- | Smart constructor for building a rule with index
rule :: (Functor f, MonadState RuleIx f)
     => f (   Spanned Term
           -> B.ByteString
           -> Spanned Term
           -> Rule)
rule = Rule <$> incState

--   XXX Having one kind of Pragma is probably wrong
data Line = LRule (Spanned Rule)
          | LPragma !(Spanned Term)
 deriving (Eq,Show)

------------------------------------------------------------------------}}}
-- Parser Configuration State                                           {{{

{-
-- | Configuration data threaded deeply into the parser
data PC m = PC { pc_opertab :: OperatorTable m (Spanned Term) }
type PCM m a = StateT (PC m) m a
-}

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

bsf :: Functor f => f String -> f B.ByteString
bsf = fmap BU.fromString

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


dynaAtomStyle :: TokenParsing m => IdentifierStyle m
dynaAtomStyle = IdentifierStyle
  { _styleName = "Atom"
  , _styleStart    = (lower <|> oneOf "$")
  , _styleLetter   = (alphaNum <|> oneOf "_'")
  , _styleReserved = H.fromList [ "is", "new", "whenever" ]
  , _styleHighlight = Constant
  , _styleReservedHighlight = ReservedOperator
  }

dynaVarStyle :: TokenParsing m => IdentifierStyle m
dynaVarStyle = IdentifierStyle
  { _styleName = "Variable"
  , _styleStart    = (upper <|> char '_')
  , _styleLetter   = (alphaNum <|> oneOf "_'")
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
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
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,Parsing,CharParsing)

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

------------------------------------------------------------------------}}}
-- Atoms                                                                {{{

atom :: (Monad m, TokenParsing m) => m B.ByteString
atom =     liftA BU.fromString stringLiteralSQ
       <|> (bsf $ ident dynaAtomStyle)

------------------------------------------------------------------------}}}
-- Terms and term expressions                                           {{{

nullaryStar :: DeltaParsing m => m (Spanned Term)
nullaryStar = spanned $ flip TFunctor [] <$> (bsf $ string "*")
                      <* (notFollowedBy $ char '(')

term :: DeltaParsing m => m (Spanned Term)
term  = token $ choice
      [       parens tfexpr
      ,       spanned $ TVar <$> (bsf $ ident dynaVarStyle)

      ,       spanned $ mkta <$> (colon *> term) <* whiteSpace <*> term

      , try $ spanned $ TString  <$> bsf stringLiteral

      , try $ spanned $ TNumeric <$> naturalOrDouble

      , try $ spanned $ flip TFunctor [] <$> atom
                      <* (notFollowedBy $ char '(')

      , try $ nullaryStar
      ,       spanned $ parenfunc
      ]
 where
  functor = highlight Identifier atom <?> "Functor"

  parenfunc = TFunctor <$> functor
                       <*>  parens (tlexpr `sepBy` symbolic ',')

  mkta ty te = TAnnot (AnnType ty) te

-- | Sometimes we require that a character not be followed by whitespace
-- and satisfy some additional predicate before we pass it off to some other parser.
thenAny :: (TokenParsing m, Monad m) => m a -> m Char
thenAny p =    anyChar                             -- some character
            <* lookAhead (notFollowedBy someSpace) -- not followed by space
            <* lookAhead p                         -- and not follwed by the request

-- | A "dot operator" is a dot followed immediately by something that looks
-- like a typical operator.  We 'lookAhead' here to avoid the case of a dot
-- by itself as being counted as an operator; the dot operator is required
-- to have not-a-space following (to avoid confusion with the end-of-rule
-- marker, which is taken to be "dot space" or "dot eof").
dotOper :: (Monad m, TokenParsing m) => m [Char]
dotOper = try (lookAhead (thenAny anyChar) *> identNL dynaDotOperStyle)

-- | A "comma operator" is a comma necessarily followed by something that
-- would continue to be an operator (i.e. punctuation).
commaOper :: (Monad m, TokenParsing m) => m [Char]
commaOper = try (   lookAhead (thenAny $ _styleLetter dynaCommaOperStyle)
                 *> identNL dynaCommaOperStyle)

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

-- | The basic expression table
--
-- XXX right now all binops are at equal precedence and left-associative;
-- that's wrong.
--
-- XXX I remember now why we didn't handle ',' as an operator: if it were,
-- we'd have no way of distinguishing between @f(a,b)@ as
--
--   > TFunctor "f" [TFunctor "a" [] :~ _, TFunctor "b" [] :~ _]
--
-- and
--
--   > TFunctor "f" [TFunctor "," [TFunctor "a" [] :~ _, TFunctor "b" [] :~ _] :~ _]
--
-- COMMAOP
-- We can fix this, but it means that we should have a separate expression
-- parser for contexts where "comma means argument separation" and "comma
-- means evaluation separator".  I don't yet know how I feel about
-- the "whenever" (and "is"?) operator(s) being available in the former table.
--
-- XXX timv suggests that this should be assocnone for binops as a quick
-- fix.  Eventually we should still do this properly.
termETable :: DeltaParsing m => [[Operator m (Spanned Term)]]
termETable = [ [ Prefix $ uf (spanned $ bsf $ symbol "new") ]
             , [ Prefix $ uf (spanned $ bsf $ ident dynaPfxOperStyle)        ]
             , [ Infix  (bf (spanned $ bsf $ ident dynaOperStyle)) AssocLeft ]
             , [ Infix  (bf (spanned $ bsf $ dotOper)) AssocRight ]
             , [ Infix  (bf (spanned $ bsf $ commaOper)) AssocRight ]
             ]

tlexpr :: DeltaParsing m => m (Spanned Term)
tlexpr = buildExpressionParser termETable term <?> "Limited Expression"


fullETable = [ [ Infix  (bf (spanned $ bsf $ symbol "is"      )) AssocNone  ]
             , [ Infix  (bf (spanned $ bsf $ symbol ","       )) AssocRight ]
             , [ Infix  (bf (spanned $ bsf $ symbol "whenever")) AssocNone  ]
             ]

tfexpr :: DeltaParsing m => m (Spanned Term)
tfexpr = buildExpressionParser fullETable tlexpr <?> "Expression"

dterm, dtlexpr, dtfexpr :: DeltaParsing m => m (Spanned Term)
dterm   = unDL term
dtlexpr = unDL tlexpr
dtfexpr = unDL tfexpr

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

{-
-- | Grab the head (term!) and aggregation operator from a line that
-- we hope is a rule.
rulepfx :: (MonadState RuleIx m, DeltaParsing m)
        => m ([Spanned Term] -> Spanned Term -> Rule)
rulepfx = rule <*> term
               <*  whiteSpace
               <*> (bsf $ ident dynaAggStyle <?> "Aggregator")
-}

parseRule :: (MonadState RuleIx m, DeltaParsing m) => m Rule
parseRule = choice [

{-
               -- HEAD OP= RESULTEXPR whenever EXPRS .
               (try (liftA flip rulepfx
                          <*> tlexpr
                          <*  hrss "whenever"))
                          <*> (tlexpr `sepBy1` symbolic ',')

               -- HEAD OP= EXPRS, RESULTEXPR .
             , try (rulepfx
                          <*> many (try (tlexpr <* symbolic ','))
                          <*> tlexpr)
-}

               try $ rule <*> term 
                          <*  whiteSpace
                          <*> (bsf $ ident dynaAggStyle <?> "Aggregator")
                          <*> tfexpr

               -- HEAD .
               -- timv: using ':-' as the "default" aggregator for facts is
               -- probably incorrect because it conflicts with '&=' and other
               -- logical aggregators.
             , do
                  h@(_ :~ s) <- term
                  ix <- get
                  return $ Rule ix h ":-" (TFunctor "true" [] :~ s)
             ]
       <* optional (char '.')
 where
  hrss = highlight ReservedOperator . spanned . symbol

drule :: (DeltaParsing m) => m (Spanned Rule)
drule = evalStateT (unDL (spanned parseRule)) 0

------------------------------------------------------------------------}}}
-- Lines                                                                {{{

dpragma :: DeltaParsing m => m (Spanned Term)
dpragma =    symbol ":-"
          *> whiteSpace
          *> tlexpr
          <* whiteSpace
          <* optional (char '.')

progline :: (MonadState RuleIx m, DeltaParsing m) => m (Spanned Line)
progline  =    whiteSpace
            *> spanned (choice [ LRule <$> spanned parseRule
                               , LPragma <$> dpragma
                               ])

dline :: (DeltaParsing m) => m (Spanned Line)
dline = evalStateT (unDL (progline <* optional whiteSpace)) 0

-- XXX This is not prepared for parser-altering pragmas.  We will have to
-- fix that.
dlines :: DeltaParsing m => m [Spanned Line]
dlines = evalStateT (unDL (many (progline <* optional whiteSpace))) 0

------------------------------------------------------------------------}}}
