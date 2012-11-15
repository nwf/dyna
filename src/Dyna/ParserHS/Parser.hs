---------------------------------------------------------------------------
-- | A parser for some chunk of the Dyna language, using Trifecta
--
-- Based in part on
-- <https://github.com/ekmett/trifecta/blob/master/examples/RFC2616.hs>
-- as well as the trifecta code itself
--
-- TODO (XXX):
--
--   * We might want to use T.T.Literate, too, in the end.
--
--   * Doesn't understand dynabase literals ("{ ... }")
--
--   * Doesn't handle parenthesized aggregators
--
--   * Doesn't handle shared subgoals ("whenever ... { ... }")
--
--   * Don't end numerics with ., even if it's the end-of-rule marker;
--   put a space first.

--   Header material                                                      {{{

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

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
import qualified Data.CharSet                     as CS
import qualified Data.HashSet                     as H
import           Data.Semigroup ((<>))
import           Data.Monoid (mempty)
import           Text.Parser.Expression
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta

import           Dyna.Term.TTerm (Annotation(..))
import           Dyna.XXX.Trifecta (identNL)

------------------------------------------------------------------------}}}
-- Parsed output definition                                             {{{

data Term = TFunctor !B.ByteString
                     ![Spanned Term]
          | TAnnot   !(Annotation (Spanned Term))
                     !(Spanned Term)
          | TNumeric !(Either Integer Double)
          | TVar     !B.ByteString
 deriving (Eq,Ord,Show)


-- | Rules are not just terms because we want to make it very syntactically
--   explicit about the head being a term (though that's not an expressivity
--   concern -- just use the parenthesized texpr case) so that there is no
--   risk of parsing ambiguity.
--
--   XXX The span on Fact is a little silly
data Rule = Fact (Spanned Term)
          | Rule !(Spanned Term) !B.ByteString ![Spanned Term] !(Spanned Term)
 deriving (Eq,Show)

--   XXX The span on LRule is a little silly
--   XXX Having one kind of Pragma is probably wrong
data Line = LRule (Spanned Rule)
          | LPragma !(Spanned Term)
 deriving (Eq,Show)


------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

bsf :: Functor f => f String -> f B.ByteString
bsf = fmap BU.fromString

------------------------------------------------------------------------}}}
-- Identifier Syles                                                     {{{

-- | The full laundry list of punctuation symbols we "usually" mean.
usualpunct :: CS.CharSet
usualpunct = CS.fromList "!#$%&*+/<=>?@\\^|-~:."

-- | Dot operators
dynaDotOperStyle :: TokenParsing m => IdentifierStyle m
dynaDotOperStyle = IdentifierStyle
  { styleName = "Dot Operator"
  , styleStart   = char '.'
  , styleLetter  = oneOfSet $ usualpunct
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
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
  { styleName = "Prefix Operator"
  , styleStart   = oneOfSet $ usualpunct CS.\\ CS.fromList ".:"
  , styleLetter  = oneOfSet $ usualpunct
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

-- | Infix operators
--
-- Dot is handled specially elsewhere due to its
-- dual purpose as an operator and rule separator.
dynaOperStyle :: TokenParsing m => IdentifierStyle m
dynaOperStyle = IdentifierStyle
  { styleName = "Infix Operator"
  , styleStart   = oneOfSet $ CS.delete '.' usualpunct
  , styleLetter  = oneOfSet $ usualpunct
  , styleReserved = mempty
  , styleHighlight = Operator
  , styleReservedHighlight = ReservedOperator
  }

dynaAtomStyle :: TokenParsing m => IdentifierStyle m
dynaAtomStyle = IdentifierStyle
  { styleName = "Atom"
  , styleStart    = (lower <|> oneOf "$")
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


------------------------------------------------------------------------}}}
-- Comment handling                                                     {{{

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

------------------------------------------------------------------------}}}
-- Atoms                                                                {{{

atom :: (Monad m, TokenParsing m) => m B.ByteString
atom =     liftA BU.fromString stringLiteral
       <|> (bsf $ ident dynaAtomStyle)

------------------------------------------------------------------------}}}
-- Terms and term expressions                                           {{{

term :: DeltaParsing m => m (Spanned Term)
term  = token $ choice
      [       parens texpr
      ,       spanned $ TVar <$> (bsf $ ident dynaVarStyle)

      ,       spanned $ mkta <$> (colon *> term) <* spaces <*> term

      , try $ spanned $ TNumeric <$> naturalOrDouble

      , try $ spanned $ flip TFunctor [] <$> atom
                      <* (notFollowedBy $ char '(')

      , try $ spanned $ flip TFunctor [] <$> (bsf $ string "*")
      ,       spanned $ parenfunc
      ]
 where
  functor = highlight Identifier atom <?> "Functor"

  parenfunc = TFunctor <$> functor
                       <*>  parens (texpr `sepBy` symbolic ',')

  mkta ty te = TAnnot (AnnType ty) te

-- XXX right now all binops are at equal precedence and left-associative; that's wrong.
texpr :: DeltaParsing m => m (Spanned Term)
texpr = buildExpressionParser etable term <?> "Expression"
 where
  etable = [ [ Prefix $ uf (spanned $ bsf $ symbol "new") ]
           , [ Prefix $ uf (spanned $ bsf $ ident dynaPfxOperStyle)        ]
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


dterm, dtexpr :: DeltaParsing m => m (Spanned Term)
dterm  = unDL term
dtexpr = unDL texpr

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

-- | Grab the head (term!) and aggregation operator from a line that
-- we hope is a rule.
rulepfx :: DeltaParsing f => f ([Spanned Term] -> Spanned Term -> Rule)
rulepfx = Rule <$> term
               <*  spaces
               <*> ((bsf $ some $ satisfy $ not . isSpace) <?> "Aggregator")  -- XXX probably a better way to do this.. probably want aggregators have suffix =
               <*  spaces

rule :: DeltaParsing m => m Rule
rule = choice [
                -- HEAD OP= RESULT EXPR whenever EXPRS .
               (try (liftA flip rulepfx
                           <*> texpr
                           <*  hrss "whenever"))
                           <*> (texpr `sepBy1` symbolic ',')

                -- HEAD OP= EXPRS, RESULT EXPR .
              , (try rulepfx)
                           <*> many (try (texpr <* symbolic ','))
                           <*> texpr

                -- HEAD .
              , Fact   <$> term
              ]
 where
  hrss = highlight ReservedOperator . spanned . symbol

drule :: DeltaParsing m => m (Spanned Rule)
drule = spanned rule

------------------------------------------------------------------------}}}
-- Lines                                                                {{{

progline :: DeltaParsing m => m (Spanned Line)
progline  = spanned $ choice [ LRule <$> drule
                             , LPragma <$> (symbol ":-" *> spaces *> texpr)
                             ]

dline :: DeltaParsing m => m (Spanned Line)
dline = unDL (progline <* optional (char '.') <* optional newline)

dlines :: DeltaParsing m => m [Spanned Line]
dlines = unDL (progline `sepEndBy` (char '.' <* spaces))

------------------------------------------------------------------------}}}
