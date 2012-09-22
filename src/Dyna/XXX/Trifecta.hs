{-# LANGUAGE RankNTypes #-}
-- XXX contribute back to trifecta

module Dyna.XXX.Trifecta (
    identNL, pureSpanned, triInteract
) where

import           Control.Applicative
import           Control.Monad (when)
import qualified Data.ByteString.UTF8                as BU
import           Data.Monoid (mempty)
import           Data.HashSet as HashSet (member)
import qualified Data.Semigroup.Reducer              as R
import           Text.Trifecta
import           Text.Trifecta.Delta

-- import Debug.Trace

-- | Just like ident but without the "token $" prefix
--
-- belongs in Text.Parser.Token
--
identNL :: (Monad m, TokenParsing m) => IdentifierStyle m -> m String
identNL s = try $ do
  name <- highlight (styleHighlight s) ((:) <$> styleStart s <*> many (styleLetter s) <?> styleName s)
  when (HashSet.member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name

-- | Just like "pure" but right here in the parsing state
--
-- belongs in Text.Trifecta.Diagnostic.Rendering.Span
pureSpanned :: DeltaParsing m => a -> m (Spanned a)
pureSpanned r = (liftA (r :~) $ Span <$> position <*> position <*> line)


-- | A multi-line interaction mechanism, for the REPL.
--
-- Maybe this should not be contributed, but it uses so much of the
-- internals that it surely belongs here beside the other such.
triInteract :: (Monad m, Show a)
            => (Parser a)                 -- ^ Parser
            -> (m (Maybe String))         -- ^ Continuation callback
            -> (a -> m b)                 -- ^ Success callback
            -> (TermDoc -> m b)           -- ^ Failure callback
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


