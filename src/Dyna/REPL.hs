
module Dyna.REPL where

import           Control.Applicative ((<*))
import           Control.Monad.Trans (liftIO)
import           System.Console.Haskeline
import           Text.PrettyPrint.Free
import           Text.Trifecta

import qualified Dyna.ParserHS.Parser      as DP
-- import qualified Dyna.NormalizeParse       as DNP
import           Dyna.XXX.Trifecta

main :: IO () 
main = do
   runInputT defaultSettings loop
 where
     loop = do
             maybeLine <- getInputLine "Dyna> "
             case maybeLine of
               Nothing -> return () -- ctrl-D
               Just l -> triInteract (DP.dline <* eof)
                                        promptCont
                                        success
                                        failure
                                        l

		 -- Interaction interprets a ^D in nested context
		 -- as an excuse to print out parsing errors
		 -- (i.e. it why it rejected the line thus far);
     -- TODO is that what we want?
     promptCont = getInputLine "      "

     success a = do
                   outputStrLn $ "\nParsed: " ++ show a
                   loop

     failure td = do
                   liftIO $ displayLn td
                   loop
