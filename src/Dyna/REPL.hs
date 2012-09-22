{-# LANGUAGE Rank2Types #-}
module Dyna.REPL where

import           Control.Applicative ((<*))
import           System.Console.Editline
import           Text.Trifecta

import qualified Dyna.ParserHS.Parser      as DP
-- import qualified Dyna.NormalizeParse       as DNP
import           Dyna.XXX.Trifecta


main :: IO () 
main = do
   el <- elInit "dyna"
   setEditor el Emacs
   let
     loop = do
             setPrompt el (return "Dyna> ")
             maybeLine <- elGets el
             case maybeLine of
               Nothing -> return () -- ctrl-D
               Just l -> triInteract (DP.dline <* eof)
                                        promptCont
                                        success
                                        failure
                                        l

		-- Interaction interprets a ^D in nested context
		-- as an excuse to print out parsing errors
		-- (i.e. it why it rejected the line 
     promptCont = do
                   setPrompt el (return "      ")
                   elGets el

     success a = do
                   putStrLn $ "\nParsed: " ++ show a
                   loop

     failure td = do
                   displayLn td
                   loop
   loop
