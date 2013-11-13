module CLI (main) where

import System.Environment
import Control.Applicative
import Control.Monad
import Frontend.Parser
import Frontend.Tokenizer
import Backend.Transform
import Backend.Render

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
	<=< transform
	<=< parse
	<=< tokenize
