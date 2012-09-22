{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- Based in part on
-- https://github.com/ekmett/trifecta/blob/master/examples/RFC2616.hs
-- as well as the trifecta code itself
--
-- XXX no longer handles comments due to trifecta code upgrade
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
import           Control.Monad
import           Control.Monad.Trans (MonadTrans,lift)
import qualified Data.ByteString.UTF8             as BU
import qualified Data.ByteString                  as B
import           Data.Char (isSpace)
import qualified Data.HashSet                     as H
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Text.Parser.Expression
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta

import           Dyna.XXX.Trifecta (identNL)

data Term = TFunctor {-# UNPACK #-} !B.ByteString ![Spanned Term]
          | TVar     {-# UNPACK #-} !B.ByteString
           -- | TDBLit XXX
 deriving (Eq,Ord,Show)

dynaDotOperStyle :: TokenParsing m => IdentifierStyle m
dynaDotOperStyle = IdentifierStyle
  { styleName = "Dot Operator"
  , styleStart   = char '.'
  , styleLetter  = oneOf "!#$%&*+/<=>?@\\^|-~:."
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

dynaOperStyle :: TokenParsing m => IdentifierStyle m
dynaOperStyle = IdentifierStyle
  { styleName = "Operator"
  , styleStart   = oneOf "!#$%&*+/<=>?@\\^|-~:"
  , styleLetter  = oneOf "!#$%&*+/<=>?@\\^|-~:."
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

dynaAtomStyle :: TokenParsing m => IdentifierStyle m
dynaAtomStyle = IdentifierStyle
  { styleName = "Atom"
  , styleStart    = (lower <|> digit <|> char '_')
  , styleLetter   = (alphaNum <|> oneOf "_'")
  , styleReserved = H.fromList [ "is", "new", "whenever" ]
  , styleHighlight = Constant
  , styleReservedHighlight = ReservedOperator
  }

dynaVarStyle :: TokenParsing m => IdentifierStyle m
dynaVarStyle = IdentifierStyle
  { styleName = "Variable"
  , styleStart    = (upper <|> char '_')
  , styleLetter   = (alphaNum <|> oneOf "_'")
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
  

bsf = fmap BU.fromString

atom :: (Monad m, TokenParsing m) => m B.ByteString
atom =     liftA BU.fromString stringLiteral
       <|> (bsf $ ident dynaAtomStyle)

-- sparen :: MonadParser m => m a -> m a
-- sparen = between (char '(' *> spaces) (spaces <* char ')')

term :: DeltaParsing m => m (Spanned Term)
term  = token $ choice
      [       parens texpr
      ,       spanned $ TVar <$> (bsf$ident dynaVarStyle)
      , try $ spanned $ flip TFunctor [] <$> atom <* (notFollowedBy $ char '(')
      ,       spanned $ parenfunc
	  ]
 where
  parenfunc = TFunctor <$> (highlight Identifier atom <?> "Functor")
                       <*>  parens (texpr `sepBy` symbolic ',')

-- XXX right now all binops are at equal precedence and left-associative; that's wrong.
texpr :: DeltaParsing m => m (Spanned Term)
texpr = buildExpressionParser etable term <?> "Expression"
 where
  etable = [ [ Prefix $ uf (spanned $ bsf $ symbol "new") ]
           , [ Prefix $ uf (spanned $ bsf $ ident dynaOperStyle)           ]
           , [ Infix  (bf (spanned $ bsf $ ident dynaOperStyle)) AssocLeft ]
		   , [ Infix  (bf (spanned $ bsf $ dotOper)) AssocRight ]
           , [ Infix  (bf (spanned $ bsf $ symbol "is")) AssocNone ]
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

dterm, dtexpr :: DeltaParsing m => m (Spanned Term)
dterm  = unDL term 
dtexpr = unDL texpr 

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
               <*> (bsf$ident dynaOperStyle <?> "Aggregator")

rule :: DeltaParsing m => m Rule
rule = choice [(try (liftA flip rulepfx
                           <*> texpr
                           <*  hriss "whenever"))
                           <*> (texpr `sepBy1` symbolic ',')

              , (try rulepfx)
                           <*> many (try (texpr <* symbolic ','))
                           <*> texpr

              , Fact   <$> term
              ]

drule :: DeltaParsing m => m (Spanned Rule)
drule = spanned rule

progline :: DeltaParsing m => m (Spanned Line)
progline  = spanned $ choice [ LRule <$> drule
                             , LPragma <$> (symbol ":-"
                                       *> spaces
                                       *> texpr)
                             ]

dline :: DeltaParsing m => m (Spanned Line)
-- dline = unDL (progline <* optional (char '.' <*  (spaces <|> eof)))
dline = unDL (progline <* optional (char '.') <* optional newline)

dlines :: DeltaParsing m => m [Spanned Line]
dlines = unDL (progline `sepEndBy` (char '.' <* spaces))
