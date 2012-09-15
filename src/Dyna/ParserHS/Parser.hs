{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- Based in part on
-- https://github.com/ekmett/trifecta/blob/master/examples/RFC2616.hs
-- as well as the trifecta code itself
--
-- TODO:
--  We might want to use T.T.Literate, too, in the end.
--  Doesn't understand dynabase literals ("{ ... }")
--  Doesn't handle parenthesized aggregators
--  Doesn't handle shared subgoals ("whenever ... { ... }")
--  Doesn't understand "foo." style rules.

module Dyna.ParserHS.Parser (
    Term(..), dterm, dtexpr,
    Rule(..), drule, Line(..), dline, dlines
) where

import           Control.Applicative
import qualified Data.ByteString.UTF8             as BU
import qualified Data.ByteString                  as B
import           Data.Char (isSpace)
import qualified Data.HashSet                     as H
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Text.Trifecta
import           Text.Trifecta.Highlight.Prim
import           Text.Trifecta.Parser.Expr
import           Text.Trifecta.Parser.Token.Style

import           Dyna.XXX.Trifecta (identNL, pureSpanned)

data Term = TFunctor {-# UNPACK #-} !B.ByteString ![Spanned Term]
          | TVar     {-# UNPACK #-} !B.ByteString
           -- | TDBLit XXX
 deriving (Eq,Ord,Show)

dynaDotOperStyle :: MonadParser m => IdentifierStyle m
dynaDotOperStyle = IdentifierStyle
  { styleName = "Dot Operator"
  , styleStart   = () <$ char '.'
  , styleLetter  = () <$ oneOf "!#$%&*+/<=>?@\\^|-~:."
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

dynaOperStyle :: MonadParser m => IdentifierStyle m
dynaOperStyle = IdentifierStyle
  { styleName = "Operator"
  , styleStart   = () <$ oneOf "!#$%&*+/<=>?@\\^|-~:"
  , styleLetter  = () <$ oneOf "!#$%&*+/<=>?@\\^|-~:."
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

dynaAtomStyle :: MonadParser m => IdentifierStyle m
dynaAtomStyle = IdentifierStyle
  { styleName = "Atom"
  , styleStart    = () <$ (lower <|> digit <|> char '_')
  , styleLetter   = () <$ (alphaNum <|> oneOf "_'")
  , styleReserved = H.fromList [ "is", "new", "whenever" ]
  , styleHighlight = Constant
  , styleReservedHighlight = ReservedOperator
  }

dynaVarStyle :: MonadParser m => IdentifierStyle m
dynaVarStyle = IdentifierStyle
  { styleName = "Variable"
  , styleStart    = () <$ (upper <|> char '_')
  , styleLetter   = () <$ (alphaNum <|> oneOf "_'")
  , styleReserved = mempty
  , styleHighlight = Identifier
  , styleReservedHighlight = ReservedIdentifier
  }

dynaCommentStyle :: CommentStyle
dynaCommentStyle =  CommentStyle
  { commentStart = "{%" -- XXX?
  , commentEnd   = "%}" -- XXX?
  , commentLine  = "%"
  , commentNesting = True
  }

dynaLanguage :: (MonadParser m)
             => LanguageDef m
dynaLanguage =  LanguageDef
  { languageCommentStyle    = dynaCommentStyle
  , languageIdentifierStyle = undefined -- dynaAtomStyle (XXX)
  , languageOperatorStyle   = undefined -- dynaOperStyle (XXX)
  }

atom :: MonadParser m => m B.ByteString
atom =     liftA BU.fromString stringLiteral
       <|> ident dynaAtomStyle

-- sparen :: MonadParser m => m a -> m a
-- sparen = between (char '(' *> spaces) (spaces <* char ')')

term :: MonadParser m => m (Spanned Term)
term  = lexeme $ choice
      [       parens texpr
      ,       spanned $ TVar <$> (ident dynaVarStyle)
      , try $ spanned $ flip TFunctor [] <$> atom <* (notFollowedBy $ char '(')
      ,       spanned $ parenfunc
	  ]
 where
  parenfunc = TFunctor <$> (highlight Identifier atom <?> "Functor")
                       <*>  parens (texpr `sepBy` symbolic ',')

-- XXX right now all binops are at equal precedence and left-associative; that's wrong.
texpr :: MonadParser m => m (Spanned Term)
texpr = buildExpressionParser etable term <?> "Expression"
 where
  etable = [ [ Prefix $ uf (spanned $ symbol "new") ]
           , [ Prefix $ uf (spanned $ ident dynaOperStyle)           ]
           , [ Infix  (bf (spanned $ ident dynaOperStyle)) AssocLeft ]
		   , [ Infix  (bf (spanned $ dotOper)) AssocRight ]
           , [ Infix  (bf (spanned $ symbol "is")) AssocNone ]
           ]

    -- The dot operator is required to have not-a-space following (to avoid
    -- confusion with the end-of-rule marker, which is taken to be "dot space"
    -- or "dot eof").
  dotAny  = char '.' <* satisfy (not . isSpace)
  dotOper = try (lookAhead dotAny *> identNL dynaDotOperStyle)

  uf f = do
    (x:~spx)  <- f
    pure (\a@(_:~sp)   -> (TFunctor x [a]):~(spx <> sp))
  bf f = do
    (x:~spx)  <- f
    pure (\a@(_:~spa) b@(_:~spb) -> (TFunctor x [a,b]):~(spa <> spx <> spb))

hriss = highlight ReservedOperator . spanned . symbol 

dynafy :: MonadParser m => Language m a -> m a
dynafy = flip runLanguage dynaLanguage

dterm, dtexpr :: MonadParser m => m (Spanned Term)
dterm  = dynafy term
dtexpr = dynafy texpr

-- | Rules are not just terms because we want to make it very syntactically
--   explicit about the head being a term (though that's not an expressivity
--   concern -- just use the parenthesized texpr case) so that there is no
--   risk of parsing ambiguity.
--
--   XXX The span on Fact is a little silly
data Rule = Fact (Spanned Term)
          | Rule !(Spanned Term) !B.ByteString ![Spanned Term] !(Spanned Term)
 deriving (Eq,Ord,Show)

--   XXX The span on LRule is a little silly
--   XXX Having one kind of Pragma is probably wrong
data Line = LRule (Spanned Rule)
          | LPragma !(Spanned Term)
 deriving (Eq,Ord,Show)

rulepfx = Rule <$> term
               <*  spaces
               <*> (ident dynaOperStyle <?> "Aggregator")

rule = choice [(try (liftA flip rulepfx
                           <*> texpr
                           <*  hriss "whenever"))
                           <*> (texpr `sepBy1` symbolic ',')

              , (try rulepfx)
                           <*> many (try (texpr <* symbolic ','))
                           <*> texpr

              , Fact   <$> term
              ]

drule = spanned rule

progline :: MonadParser m => m (Spanned Line)
progline  = spanned $ choice [ LRule <$> drule
                             , LPragma <$> (symbol ":-"
                                       *> spaces
                                       *> texpr)
                             ]

dline :: MonadParser m => m (Spanned Line)
-- dline = dynafy (progline <* optional (char '.' <*  (spaces <|> eof)))
dline = dynafy (progline <* optional (char '.') <* optional newline)

dlines :: MonadParser m => m [Spanned Line]
dlines = dynafy (progline `sepEndBy` (char '.' <* spaces))
