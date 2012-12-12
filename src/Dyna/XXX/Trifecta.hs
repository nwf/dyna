{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- XXX contribute back to trifecta

module Dyna.XXX.Trifecta (
    identNL, pureSpanned, stringLiteralSQ, triInteract, renderSpan
) where

import           Control.Applicative
import           Control.Monad (when)
import qualified Data.ByteString.UTF8                as BU
import           Data.Char
import           Data.List (foldl')
import           Data.Monoid (mempty)
import           Data.HashSet as HashSet (member)
import qualified Data.Semigroup.Reducer              as R
import           Text.Parser.Token.Highlight
import           Text.Trifecta
import           Text.Trifecta.Delta

import           Text.PrettyPrint.Free ((<+>), above, indent)

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

-- | Just like stringLiteral but with single quotes.
--
-- belongs in Text.Parser.Token
stringLiteralSQ :: TokenParsing m => m String
stringLiteralSQ = token (highlight StringLiteral lit) where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '\'') (char '\'' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))
                    -- XXX That is, charLetter

  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteralSQ #-}

-- XXX Duplicated from Text.Parser.Token
escapeCode :: TokenParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum 'A')) <$> (char '^' *> upper)
  charNum     = toEnum . fromInteger <$> num where
    num = decimal
      <|> (char 'o' *> number 8 octDigit)
      <|> (char 'x' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: [Char]
  ascii2 = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI'
           ,'\EM','\FS','\GS','\RS','\US','\SP']
  ascii3 = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK'
           ,'\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK'
           ,'\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

-- XXX Duplicated from Text.Parser.Token
number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit


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


------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

renderSpan (Span s e bs) =
       prettyTerm s
   <+> "-"
   <+> prettyTerm e
   <+> ":"
   `above` (indent 2 (prettyTerm $ rendering s bs))
