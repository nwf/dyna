---------------------------------------------------------------------------
-- | The \"practical\" side of Dyna's syntax.

-- Header material                                                      {{{
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Dyna.ParserHS.Syntax where

import           Control.Applicative
import qualified Data.ByteString.UTF8             as BU
import qualified Data.CharSet                     as CS
import qualified Data.Map                         as M
import           Data.Monoid (mempty)
import           Data.Semigroup ((<>))
import           Data.String
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.ParserHS.Types
import           Dyna.Term.TTerm
import           Dyna.XXX.Trifecta (identNL)
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta

------------------------------------------------------------------------}}}
-- Common definitions                                                   {{{

-- | The full laundry list of punctuation symbols we "usually" mean.
usualpunct :: CS.CharSet
usualpunct = CS.fromList "!#$%&*+/<=>?@\\^|-~:.,"

-- | Sometimes we require that a character not be followed by whitespace
-- and satisfy some additional predicate before we pass it off to some other parser.
thenAny :: (Monad m, TokenParsing m, LookAheadParsing m)
        => m a -> m Char
thenAny p =    anyChar                             -- some character
            <* lookAhead (notFollowedBy someSpace) -- not followed by space
            <* lookAhead p                         -- and not follwed by the request

------------------------------------------------------------------------}}}
-- Operator identifier styles and parsers                               {{{

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

-- | Prefix operators also handled by trifecta's built-in handling
prefixOper :: (Monad m, TokenParsing m) => m String
prefixOper = ident dynaPfxOperStyle

-- | Infix operators
--
-- Dot is handled specially elsewhere due to its
-- dual purpose as an operator and rule separator.
-- Comma similarly has special handling due to its
-- nature as term and subgoal separator.
dynaOperStyle :: (TokenParsing m, Monad m) => IdentifierStyle m
dynaOperStyle = IdentifierStyle
  { _styleName = "Infix Operator"
  , _styleStart   = oneOfSet $ usualpunct CS.\\ CS.fromList ".,"
  , _styleLetter  = oneOfSet (usualpunct CS.\\ CS.fromList ".")
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | A normal operator is handled by trifecta's built-in handling
infixOper :: (Monad m, TokenParsing m) => m String
infixOper = ident dynaOperStyle

-- | Dot operator definition; use 'dotOper'
dynaDotOperStyle :: TokenParsing m => IdentifierStyle m
dynaDotOperStyle = IdentifierStyle
  { _styleName = "Dot-Operator"
  , _styleStart   = char '.'
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | A "dot operator" is a dot followed immediately by something that looks
-- like a typical operator.  We 'lookAhead' here to avoid the case of a dot
-- by itself as being counted as an operator; the dot operator is required
-- to have not-a-space following (to avoid confusion with the end-of-rule
-- marker, which is taken to be "dot space" or "dot eof").
dotOper :: (Monad m, TokenParsing m, LookAheadParsing m)
        => m String
dotOper = try (lookAhead (thenAny anyChar) *> identNL dynaDotOperStyle)

-- | Comma operator definition; use 'commaOper'
dynaCommaOperStyle :: TokenParsing m => IdentifierStyle m
dynaCommaOperStyle = IdentifierStyle
  { _styleName = "Comma-Operator"
  , _styleStart   = char ','
  , _styleLetter  = oneOfSet $ usualpunct
  , _styleReserved = mempty
  , _styleHighlight = Operator
  , _styleReservedHighlight = ReservedOperator
  }

-- | A "comma operator" is a comma necessarily followed by something that
-- would continue to be an operator (i.e. punctuation).
commaOper :: (Monad m, TokenParsing m, LookAheadParsing m)
          => m String
commaOper = try (   lookAhead (thenAny $ _styleLetter dynaCommaOperStyle)
                       *> identNL dynaCommaOperStyle)


------------------------------------------------------------------------}}}
-- Operator Specification Template                                      {{{
-- Utilities                                                            {{{

{-
ostInf :: forall f m at st.
          (Applicative m,
             st ~ Spanned Term,
             at ~ (ArgDispos -> st))
       => (SelfDispos -> ArgDispos -> f st -> TermF st)
       -> (st -> st -> f (st))
       -> DisposTab (String,Int)
       -> m String
       -> m (at -> at -> at)
ostInf tf f dt o = (pure fn <*> o)
 where
  fn s x y e = let (ds,[d1,d2]) = dt ((s,2),DTPInfix)
                   l@(_ :~ sl)  = x d1
                   r@(_ :~ sr)  = y d2
               in Term (tf ds e (f l r)) :~ (sl <> sr)

ostPre :: forall f m at st.
          (Applicative m,
             st ~ Spanned Term,
             at ~ (ArgDispos -> st))
       => (SelfDispos -> ArgDispos -> f st -> TermF st)
       -> (st -> f (st))
       -> DisposTab (String,Int)
       -> m (Spanned String)
       -> m (at -> at)
ostPre tf f dt o = (pure fn <*> o)
 where
  fn (s :~ ss) x e = let (ds,[d1]) = dt ((s,1),DTPPrefix)
                         l@(_ :~ sl)  = x d1
                     in Term (tf ds e (f l)) :~ (sl <> ss)

ostPost :: forall f m at st.
           (Applicative m,
              st ~ Spanned Term,
              at ~ (ArgDispos -> st))
        => (SelfDispos -> ArgDispos -> f st -> TermF st)
        -> (st -> f (st))
        -> DisposTab (String,Int)
        -> m (Spanned String)
        -> m (at -> at)
ostPost tf f dt o = (pure fn <*> o)
 where
  fn (s :~ ss) x e = let (ds,[d1]) = dt ((s,1),DTPPostfix)
                         r@(_ :~ sr)  = x d1
                     in Term (tf ds e (f r)) :~ (ss <> sr)

-- | An evaluated quotation marker takes itself out of the parse tree, while
-- a quoted one sticks around as a 'TFunctor' and acts as the disposition
-- table directs.
ostQuote :: forall f m at st.
            (Applicative m,
               st ~ Spanned Term,
               at ~ (ArgDispos -> st))
         => DisposTab (String,Int)
         -> m (Spanned String)
         -> m (at -> at)
ostQuote dt o = (pure fn <*> o)
 where
  fn _         x ADEval  = x ADQuote
  fn (s :~ ss) x ADQuote = let (ds,[d1]) = dt ((s,1),DTPPrefix)
                               q@(_ :~ sq) = x d1
                           in Term (TFunctor ds ADQuote (BU.fromString s) [q])
                               :~ (ss <> sq)
-}

mkArgOS :: OperSpec -> OperSpec
mkArgOS m = foldr M.delete m [",", "for", "whenever", "is", "=", "=="]

mkListOS :: OperSpec -> OperSpec
mkListOS m = foldr M.delete (mkArgOS m) ["|"]

------------------------------------------------------------------------}}}

dynaOperSpec :: OperSpec
dynaOperSpec = M.fromList $
  [ ("&"       , (Just 9 , Nothing, Just (3, AssocLeft ))) -- DPQuote  DPBiAnd
  , ("-"       , (Just 9 , Nothing, Just (6, AssocLeft ))) -- DPUnNNeg DPBiSub
  , ("|"       , (Nothing, Nothing, Just (2, AssocRight))) -- DPBiOr
  , ("%"       , (Nothing, Nothing, Just (3, AssocLeft ))) -- DPBiMod
  , ("+"       , (Nothing, Nothing, Just (6, AssocLeft ))) -- DPBiAdd
  , ("/"       , (Nothing, Nothing, Just (7, AssocLeft ))) -- DPBiDiv
  , ("^"       , (Nothing, Nothing, Just (8, AssocLeft ))) -- DPBiExp
  , ("**"      , (Nothing, Nothing, Just (8, AssocLeft )))
  , ("*"       , (Just 9 , Nothing, Just (7, AssocLeft ))) -- DPEval   DPBiMul
  , ("!"       , (Just 9 , Nothing, Nothing             )) -- DPUnLNeg


  , ("<"       , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpLt
  , ("<="      , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpLe
  , ("=="      , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpEq
  , ("="       , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpEq
  , (">="      , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpGe
  , (">"       , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpGt
  , ("!="      , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiCmpNe

  , (","       , (Nothing, Nothing, Just (1, AssocRight))) -- DPBiConj
  , ("for"     , (Nothing, Nothing, Just (0, AssocLeft )))
  , ("whenever", (Nothing, Nothing, Just (0, AssocLeft )))

  , ("in"      , (Nothing, Nothing, Just (4, AssocNone ))) -- DPBiIn
  , ("is"      , (Nothing, Nothing, Just (2, AssocNone ))) -- DPBiIs
  , ("new"     , (Just 0 , Nothing, Nothing             )) -- DPUnNew
  , ("->"      , (Nothing, Nothing, Just (7, AssocNone ))) -- DPMapsTo
  , ("with_key", (Nothing, Nothing, Just (4, AssocNone ))) -- DPWithKey
  ]

------------------------------------------------------------------------}}}
-- Default Disposition tables                                           {{{

-- | The default Dyna disposition table.
defDispos_dyna :: DisposTab (BU.ByteString,Int)
defDispos_dyna (fa@(_,a),p) = maybe (SDInherit, replicate a ADEval) id l
 where
  l = case (fa,p) of
        -- misc
        (("=",2),_)              -> Just $ (SDEval ,[ADQuote, ADQuote])
        (("pair" ,2),DTPFunctor) -> Just $ (SDQuote,[ADEval ,ADEval ])

        -- booleans
        (("true",0),DTPFunctor)  -> Just $ (SDQuote,[])
        (("false",0),DTPFunctor) -> Just $ (SDQuote,[])

        -- lists
        (("nil",  0),DTPFunctor) -> Just $ (SDQuote,[])
        (("cons", 2),DTPFunctor) -> Just $ (SDQuote,[ADEval,ADEval])

        -- key
        (("$key",1),DTPFunctor)  -> Just $ (SDEval, [ADQuote])
        (("with_key",2),DTPInfix)-> Just $ (SDQuote,[ADEval , ADEval ])

        -- tuples
        (("->",2),DTPInfix)      -> Just $ (SDQuote,[ADQuote, ADQuote])
        ((":",2),DTPInfix)       -> Just $ (SDQuote,[ADEval, ADEval])

        _                        -> Nothing

------------------------------------------------------------------------}}}
-- Aggregation operators                                                {{{

{-
primAggrSpec :: AggrSpec DPrimAggr
primAggrSpec = M.fromList
  [ ("&="       , DAAnd    )
  , ("?="       , DAArb    )
  , (":="       , DALast   )
  , ("max="     , DAMax    )
  , ("majority=", DAMaj    )
  , ("mean="    , DAMean   )
  , ("min="     , DAMin    )
  , ("|="       , DAOr     )
  , ("+="       , DASum    )
  , (":-"       , DAProlog )
  , ("*="       , DAProd   )
  , ("="        , DAUnique )
  ] 
-}

------------------------------------------------------------------------}}}
-- Keywords                                                             {{{

-- These are defined here rather than being implicit in Dyna.Analysis.ANF.
--
-- If we ever revisit the structure of rules, cross-ref XREF:ANFRESERVED and
-- maybe move all of this into the parser proper.

dynaEvalOper :: (IsString s) => s
dynaEvalOper  = "*"

dynaQuoteOper :: (IsString s) => s
dynaQuoteOper = "&"

dynaEvalAssignOper :: (IsString s) => s
dynaEvalAssignOper = "is"

dynaConjOper :: (IsString s) => s
dynaConjOper = ","

dynaConsOper :: (IsString s) => s
dynaConsOper = "cons"

dynaNilOper :: (IsString s) => s
dynaNilOper = "nil"

dynaRevConjOpers :: (IsString s) => [s]
dynaRevConjOpers = ["whenever","for"]

dynaUnifOpers :: (IsString s) => [s]
dynaUnifOpers = [ "=", "==" ]

dynaUnitTerm :: TBase
dynaUnitTerm = TBool True

daPlusEq :: (IsString s) => s
daPlusEq = "+="

daProlog :: (IsString s) => s
daProlog = ":-"

------------------------------------------------------------------------}}}

