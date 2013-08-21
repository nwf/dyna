---------------------------------------------------------------------------
-- | The \"theoretical\" side of Dyna's syntax.

-- Header material                                                      {{{

{-# LANGUAGE Rank2Types #-}

module Dyna.ParserHS.SyntaxTheory where

import           Control.Applicative
import           Control.Lens
import qualified Data.Char                  as C
import qualified Data.Map                   as M
import           Data.Monoid (Endo(..))
import           Data.Semigroup ((<>))
import           Dyna.XXX.DataUtils (mapInOrCons)
import qualified Text.Parser.Char           as TP
import qualified Text.Parser.Combinators    as TP
import qualified Text.Parser.Token          as TP
import qualified Text.Parser.Expression     as TP
import           Text.Trifecta (DeltaParsing,Spanned(..),spanned)

------------------------------------------------------------------------}}}
-- Aggregator handling                                                  {{{

{-
-- | Aggregator specification, returning type @a@.
type AggrSpec a = M.Map String a

mkAggrParser :: (Monad m, TP.TokenParsing m)
             => AggrSpec a -> m (Either a BU.ByteString)
mkAggrParser s0 = liftM Left $ TP.choice $
  M.foldrWithKey (\a p -> ((TP.symbol a *> pure p):)) [] s0
-}

------------------------------------------------------------------------}}}
-- Evaluation Disposition                                               {{{
-- Definition                                                           {{{

data SelfDispos = SDInherit
                | SDEval
                | SDQuote
 deriving (Bounded,Eq,Enum,Show)

data ArgDispos = ADEval
               | ADQuote
 deriving (Bounded,Eq,Enum,Show)

data DTPosn = DTPPrefix | DTPPostfix | DTPInfix | DTPFunctor
 deriving (Bounded,Enum,Eq,Ord,Show)

type DisposTabOver f = M.Map (f,DTPosn) (SelfDispos,[ArgDispos])
type DisposTab f = (f,DTPosn) -> (SelfDispos,[ArgDispos])

------------------------------------------------------------------------}}}
-- Functions                                                            {{{

disposOverMerge :: (Ord f)
                => (f,DTPosn) -> (SelfDispos,[ArgDispos])
                -> DisposTabOver f
                -> DisposTabOver f
disposOverMerge = M.insert
{-# INLINE disposOverMerge #-}

mkDisposTab :: (Ord f) => DisposTab f -> DisposTabOver f -> DisposTab f
mkDisposTab defl spec f = maybe (defl f) id $ M.lookup f spec

------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
-- Operator table                                                       {{{

-- | Operator symbol specification, returning type @a@.
-- 
-- Each entry in the map pairs surface syntax with its optional
-- (prefix,postfix,infix) priority and interpretations (and, in the case of
-- infix operators, its associativity).  Lower priorities bind tighter.
--
-- Note that the contents of the map are expected to be /given/ their
-- parsers; see 'mkOperTab'.  For convenience, the parsed syntactic form
-- is yielded -- this is especially useful for indexing into a 'DisposTab'.
type OperSpec = M.Map String (Maybe Int, Maybe Int, Maybe (Int, TP.Assoc))

data Fixity = PFIn TP.Assoc | PFPre | PFPost
 deriving (Eq,Show)

-- Utilities                                                            {{{


operSpecMut :: String -> Fixity -> Maybe Int -> OperSpec -> OperSpec
operSpecMut sym fx = mut
 where
  mut = case fx of
             PFPre -> set (l . _1)
             PFPost -> set (l . _2)
             PFIn a -> \mi -> set (l . _3) (fmap (\i -> (i,a)) mi)
  l = at sym . _Just

mkExprParser :: (Applicative m, TP.TokenParsing m, DeltaParsing m)
             => (String -> Spanned t -> t)
             -> (String -> Spanned t -> t)
             -> (String -> Spanned t -> Spanned t -> t)
             -> OperSpec
             -> Endo (m (Spanned t))
mkExprParser pre_ post_ inf_ s
  = Endo (TP.buildExpressionParser $ interpSpec M.empty $ M.toList s)
 where
  prfx (o :~ so) x@(_ :~ sx) = pre_  o x :~ (so <> sx)
  post (o :~ so) x@(_ :~ sx) = post_ o x :~ (sx <> so)
  inf  (o :~ _)  x@(_ :~ sx) y@(_ :~ sy) = inf_ o x y :~ (sx <> sy)

  interpSpec m [] = map snd $ M.toDescList m
  interpSpec m ((o,lfs):os) = interpSpec (go lfs m) os
   where
    po = spanned (TP.token (TP.string o <* lm))

    -- Longest match rule
    lm = case o ^? _last of
            Just x | C.isPunctuation x -> TP.notFollowedBy (TP.satisfy punct)
            Just x | C.isAlpha x       -> TP.notFollowedBy (TP.satisfy C.isAlpha)
            _                          -> pure ()

     where
      -- Punctuation-ending operators are not allowed to be adjacent to
      -- other punctuation, unless the latter is OpenPunctuation or a
      -- quote character.
      punct x = C.isPunctuation x
                && not (x `elem` ['"', '\''])
                && not (C.generalCategory x `elem` [C.OpenPunctuation])

    go (mr,mo,mi) =
       maybe id (\ p    -> mapInOrCons p (TP.Prefix $ prfx <$> po)) mr
     . maybe id (\ p    -> mapInOrCons p (TP.Postfix $ post <$> po)) mo
     . maybe id (\(p,a) -> mapInOrCons p (TP.Infix (inf <$> po) a)) mi


------------------------------------------------------------------------}}}
------------------------------------------------------------------------}}}
