{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment
import Control.Applicative
import Control.Monad
import Frontend.Chlorinate
import Frontend.Parser
import Frontend.Tokenizer
import Backend.Dechlorinate
import Backend.Render
import Chloride.Vectorizer
import Chloride.IOMagic
import Success

main = do
	args <- getArgs
	case args of
		[filename1, filename2] -> processFiles filename1 filename2
		_ -> putStrLn "usage: sodium filename.pas filename.hs"

processFiles :: String -> String -> IO ()
processFiles filename1 filename2 = do
	source <- readFile filename1
	case process source of
		Success dest -> do
			putStrLn dest
			writeFile filename2 dest
		Fail _ msgs -> do
			putStrLn "Can\'t translate."
			processFail msgs

processFail :: [String] -> IO ()
processFail = \case
	[] -> putStrLn "Error unknown"
	[msg] -> putStrLn $ "Error: " ++ msg
	msgs' -> do
		putStrLn "Try fixing one of the following errors:"
		void $ forM msgs' (\msg -> putStrLn $ "\t" ++ msg)

process :: String -> (Fail String) String
process
	 =  render
	<=< dechlorinate
	<=< vectorize
	<=< uncurse
	<=< chlorinate
	<=< parse
	<=< tokenize
