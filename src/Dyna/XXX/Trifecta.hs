{-# LANGUAGE RankNTypes #-}
-- XXX contribute back to trifecta

module Dyna.XXX.Trifecta (
    identNL, pureSpanned, stepParserBS, triInteract
) where

import           Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import qualified Data.ByteString.UTF8                as BU
import           Control.Applicative
import           Control.Monad (when)
import           Data.HashSet as HashSet (member)
import           Data.Monoid
import qualified Data.Semigroup.Reducer              as R
import qualified Data.Sequence                       as Q
import           Text.Trifecta

import qualified Text.Trifecta.Parser.Step           as TPS
import qualified Text.Trifecta.Parser.Mark           as TPM

    -- XXX
import Debug.Trace

-- | Step a trifecta parser
--
-- based on Text.Trifecta.Parser.parseByteString
stepParserBS :: Show a
             => (forall r. Parser r String a)
             -> Delta
             -> ByteString
             -> TPS.Step TermDoc a
stepParserBS p d inp = TPS.feed inp $ stepParser 
                   (fmap prettyTerm)
                   (why prettyTerm)
                   (TPM.release d *> p)
                   mempty
                   True
                   mempty
                   mempty

-- | Just like ident but without the "lexeme $" prefix
--
-- belongs in Text.Trifecta.Parser.Identifier
--
identNL :: MonadParser m => IdentifierStyle m -> m ByteString
identNL s = try $ do
  name <- highlight (styleHighlight s) (sliced (styleStart s *> skipMany (styleLetter s))) <?> styleName s
  when (member name (styleReserved s)) $ unexpected $ "reserved " ++ styleName s ++ " " ++ show name
  return name

-- | Just like "pure" but right here in the parsing state
--
-- belongs in Text.Trifecta.Diagnostic.Rendering.Span
pureSpanned :: MonadParser f => a -> f (Spanned a)
pureSpanned r = (liftA (r :~) $ Span <$> position <*> position <*> line)


-- | A multi-line interaction mechanism, for the REPL.
--
-- Maybe this should not be contributed, but it uses so much of the
-- internals that it surely belongs here beside the other such.
triInteract :: (Monad m, Show a)
            => (forall m' . MonadParser m' => m' a) -- ^ Parser
            -> (m (Maybe String))                   -- ^ Continuation callback
            -> (a -> m ())                          -- ^ Success callback
            -> (Q.Seq (Diagnostic TermDoc) -> m ()) -- ^ Failure callback
            -> String                               -- ^ Initial input
            -> m ()
triInteract p c s f i = loop (stepParserBS p dd $ BU.fromString i)
 where
     loop x = traceShow ("triInteract", x) $ case x of
                TPS.StepDone _ _  a -> s a
                TPS.StepFail _  sd   -> f sd
                TPS.StepCont ro re k -> case re of
                    Success _  a -> s a
                    Failure sd   -> c >>= maybe (f sd) (loop . k . R.snoc ro)

     dd = Directed (BU.fromString "interactive") 0 0 0 0

