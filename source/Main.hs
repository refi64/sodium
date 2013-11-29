module Main (main) where

import System.Environment
import Control.Applicative
import Control.Monad
import Frontend.Chlorinate
import Frontend.Parser
import Frontend.Tokenizer
import Backend.Dechlorinate
import Backend.Render
import Chloride.Dissolve

main = do
	args <- getArgs
	case args of
		[filename1, filename2] -> do
			source <- readFile filename1
			case process source of
				Just dest -> do
					putStrLn dest
					writeFile filename2 dest
				Nothing ->
					putStrLn "Can\'t translate"
		_ -> putStrLn "usage: sodium filename.pas filename.hs"

process :: String -> Maybe String
process
	 =  render
--	 = (return . show)
	<=< dechlorinate
--	<=< dissolve
	<=< chlorinate
	<=< parse
	<=< tokenize
