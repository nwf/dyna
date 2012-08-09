-- XXX contribute back to trifecta

module Dyna.XXX.Trifecta (
    identNL, pureSpanned
) where

import           Data.ByteString as Strict hiding (map, zip, foldl, foldr)
import           Control.Applicative
import           Control.Monad (when)
import           Data.HashSet as HashSet (member)
import           Text.Trifecta

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
pureSpanned r = (liftA (r :~) $ Span <$> position <*> position <*> line)
