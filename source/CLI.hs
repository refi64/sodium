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
		[filename1, filename2]
			->  readFile filename1
			>>= process
			>>=	writeFile filename2
		_ -> putStrLn "usage: sodium filename.pas filename.hs"

process source = do
	let tokens = tokenize source
	print tokens
	let parsetree = tokens >>= parse
	print parsetree
	let modtree = transform <$> parsetree
	print modtree
	let dest = render <$> modtree
	print dest
	return $ case dest of
		Nothing -> "-- CAN\'T TRANSLATE --"
		Just cs -> cs
