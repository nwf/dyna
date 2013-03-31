{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- XXX contribute back to trifecta

-- Header material                                                      {{{
module Dyna.XXX.Trifecta (
    identNL, pureSpanned, stringLiteralSQ, triInteract, prettySpanLoc,
    unSpan
) where

import           Control.Applicative
import           Control.Monad (when)
import qualified Data.ByteString.UTF8                as BU
import           Data.Monoid (mempty)
import           Data.HashSet as HashSet (member)
import qualified Data.Semigroup.Reducer              as R
import qualified Data.Int                            as I
import           Text.Parser.Token.Highlight
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Result
import qualified Text.PrettyPrint.Free               as PP
import qualified Text.PrettyPrint.ANSI.Leijen        as PPA

-- import Debug.Trace

------------------------------------------------------------------------}}}
-- identNL                                                              {{{

-- | Just like ident but without the "token $" prefix
--
-- belongs in Text.Parser.Token
--
identNL :: (Monad m, TokenParsing m) => IdentifierStyle m -> m String
identNL s = try $ do
  name <- highlight (_styleHighlight s)
          ((:) <$> _styleStart s <*> many (_styleLetter s) <?> _styleName s)
  when (HashSet.member name (_styleReserved s)) $ unexpected $ "reserved " ++ _styleName s ++ " " ++ show name
  return name

------------------------------------------------------------------------}}}
-- String literal variants                                              {{{

-- | Just like stringLiteral but with single quotes.
--
-- belongs in Text.Parser.Token
stringLiteralSQ :: TokenParsing m => m String
stringLiteralSQ = token (highlight StringLiteral lit) where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '\'') (char '\'' <?> "end of string") (many $ Just <$> characterChar)
    <?> "string"
{-# INLINE stringLiteralSQ #-}

------------------------------------------------------------------------}}}
-- pureSpanned                                                          {{{

-- | Just like "pure" but right here in the parsing state
--
-- belongs in Text.Trifecta.Diagnostic.Rendering.Span
pureSpanned :: DeltaParsing m => a -> m (Spanned a)
pureSpanned r = (liftA (r :~) $ Span <$> position <*> position <*> line)

------------------------------------------------------------------------}}}
-- unSpan                                                               {{{

unSpan :: Spanned a -> a
unSpan (x :~ _) = x
{-# INLINE unSpan #-}

------------------------------------------------------------------------}}}
-- Interaction                                                          {{{

-- | A multi-line interaction mechanism, for the REPL.
--
-- Maybe this should not be contributed, but it uses so much of the
-- internals that it surely belongs here beside the other such.
triInteract :: (Monad m, Show a)
            => (Parser a)                 -- ^ Parser
            -> (m (Maybe String))         -- ^ Continuation callback
            -> (a -> m b)                 -- ^ Success callback
            -> (PPA.Doc -> m b)          -- ^ Failure callback
            -> String                     -- ^ Initial input
            -> m b
triInteract p c s f i = loop (feed (BU.fromString i) $ stepParser (release dd *> p) dd mempty)
 where
     loop x = {- traceShow ("triInteract", x) $ -} case x of
                StepDone _  a -> s a
                StepFail _  sd   -> f sd
                StepCont ro re k -> case re of
                    Success a    -> s a
                    Failure sd   -> c >>= maybe (f sd) (loop . k . R.snoc ro)

     dd = Directed (BU.fromString "interactive") 0 0 0 0


------------------------------------------------------------------------}}}
-- Diagnostic utilities                                                 {{{

-- XXX I'd really like (but cannot seem to get) the ability to suppress the
-- file name if it's the same in both cases.  Stripping Directed to Lines
-- results in the lie of "(interactive)".  In any case, this function is
-- here as a placeholder for doing the right thing.
prettySpanLoc :: Span -> PP.Doc e
prettySpanLoc (Span s e _) = doPretty s PP.<> PP.char '-' PP.<> doPretty e
 where
  -- This is pretty from the Pretty Delta instance of Text.Trifecta.Delta
  -- stripped of its ANSI commands so that it works with
  -- Text.PrettyPrint.Free.  Le sigh!  XXX
  doPretty d = case d of
    Columns c _ -> k f 0 c
    Tab x y _ -> k f 0 (nextTab x + y)
    Lines l c _ _ -> k f l c
    Directed fn l c _ _ -> k fn l c
   where
      k :: BU.ByteString -> I.Int64 -> I.Int64 -> PP.Doc e
      k fn ln cn =       PP.pretty fn
                   PP.<> PP.char ':'
                   PP.<> PP.pretty (ln+1)
                   PP.<> PP.char ':'
                   PP.<> PP.pretty (cn+1)
      f :: BU.ByteString
      f = "(interactive)"

------------------------------------------------------------------------}}}
