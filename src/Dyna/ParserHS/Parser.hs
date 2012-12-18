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
    Term(..), dterm, dtexpr,
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
data Rule = Rule !RuleIx !(Spanned Term) !B.ByteString ![Spanned Term] !(Spanned Term)
 deriving (Eq,Show)

-- | Smart constructor for building a rule with index
rule :: (Functor f, MonadState RuleIx f)
     => f (   Spanned Term
           -> B.ByteString
           -> [Spanned Term]
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

dynaAggStyle :: TokenParsing m => IdentifierStyle m
dynaAggStyle = IdentifierStyle
  { styleName = "Aggregator"
  , styleStart   =     (oneOfSet $ CS.delete '.' usualpunct)
                   <|> lower
  , styleLetter  =     (oneOfSet $ usualpunct)
                   <|> alphaNum
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
      [       parens texpr
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
                       <*>  parens (texpr `sepBy` symbolic ',')

  mkta ty te = TAnnot (AnnType ty) te

-- | The dot operator is required to have not-a-space following (to avoid
-- confusion with the end-of-rule marker, which is taken to be "dot space"
-- or "dot eof").
--
-- XXX dotAny is also likely useful when we get dynabase handling, but we're
-- not there yet.
dotAny :: (TokenParsing m, Monad m) => m Char
dotAny  =    char '.'							 -- is a dot
          <* lookAhead (notFollowedBy someSpace) -- not followed by space
          <* lookAhead anyChar					 -- and not follwed by EOF

-- | A "dot operator" is a dot followed immediately by something that looks
-- like a typical operator.  We 'lookAhead' here to avoid the case of a dot
-- by itself as being counted as an operator.
dotOper :: (Monad m, TokenParsing m) => m [Char]
dotOper = try (lookAhead dotAny *> identNL dynaDotOperStyle)

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
                -- XXX "is" belongs only in the full expression parser, not
                -- in the term table
             , [ Infix  (bf (spanned $ bsf $ symbol "is")) AssocNone ]
             ]

-- fullETable = termETable ++
--              [ [ Infix  (bf (spanned $ bsf $ symbol "is")) AssocNone ]
--             , [ Infix  (bf (spanned $ bsf $ symbol ",")) AssocRight ]
--             ]

texpr :: DeltaParsing m => m (Spanned Term)
texpr = buildExpressionParser termETable term <?> "Expression"

dterm, dtexpr :: DeltaParsing m => m (Spanned Term)
dterm  = unDL term
dtexpr = unDL texpr

------------------------------------------------------------------------}}}
-- Rules                                                                {{{

-- | Grab the head (term!) and aggregation operator from a line that
-- we hope is a rule.
rulepfx :: (MonadState RuleIx m, DeltaParsing m)
        => m ([Spanned Term] -> Spanned Term -> Rule)
rulepfx = rule <*> term
               <*  whiteSpace
               <*> (bsf $ ident dynaAggStyle <?> "Aggregator")

parseRule :: (MonadState RuleIx m, DeltaParsing m) => m Rule
parseRule = choice [
               -- HEAD OP= RESULTEXPR whenever EXPRS .
               (try (liftA flip rulepfx
                          <*> texpr
                          <*  hrss "whenever"))
                          <*> (texpr `sepBy1` symbolic ',')

               -- HEAD OP= EXPRS, RESULTEXPR .
             , try (rulepfx
                          <*> many (try (texpr <* symbolic ','))
                          <*> texpr)

               -- HEAD .
               -- timv: using ':-' as the "default" aggregator for facts is
               -- probably incorrect because it conflicts with '&=' and other
               -- logical aggregators.
             , do
                  h@(_ :~ s) <- term
                  ix <- get
                  return $ Rule ix h ":-" [] (TFunctor "true" [] :~ s)
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
          *> texpr
          <* whiteSpace
          <* optional (char '.')

progline :: (MonadState RuleIx m, DeltaParsing m) => m (Spanned Line)
progline  =    whiteSpace
            *> spanned (choice [ LRule <$> spanned parseRule
                               , LPragma <$> dpragma
                               ])

dline :: (DeltaParsing m) => m (Spanned Line)
dline = evalStateT (unDL (progline <* optional whiteSpace)) 0

dlines :: DeltaParsing m => m [Spanned Line]
dlines = evalStateT (unDL (many (progline <* optional whiteSpace))) 0

------------------------------------------------------------------------}}}
