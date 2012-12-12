---------------------------------------------------------------------------
-- | A variety of debugging backends.
--
-- XXX Eventually, these may want such things kicked on by flags rather
-- than invoked directly.

-- Header material                                                      {{{
module Dyna.Backend.Debugging where

import           Control.Applicative ((<*))
import           Control.Exception
import           Dyna.Analysis.ANF
import qualified Dyna.ParserHS.Parser                as P
import           System.IO
import           Text.PrettyPrint.Free               as PP
import qualified Text.Trifecta                       as T

------------------------------------------------------------------------}}}
-- File to ANF                                                          {{{

-- | Normalize all the rules in a file and emit S-exprs for the ANF
-- normalized form.
--
-- NOTE: This is used by bin/prototype.py
normalizeFile_ file oh = do
  pr <- T.parseFromFileEx (P.dlines <* T.eof) file
  case pr of
    T.Failure td -> T.display td
    T.Success rs -> mapM_ (PP.hPutDoc oh)
                    $ map (\(P.LRule x T.:~ _) -> printANF $ normRule x) rs

normalizeFile i = bracket
  (openFile (i++".anf") WriteMode)
  (hClose)
  $ normalizeFile_ i

normalizeFileStdout file = normalizeFile_ file stdout

------------------------------------------------------------------------}}}
