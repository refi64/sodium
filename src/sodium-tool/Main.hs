module Main (main) where

import System.Environment
import Control.Monad
import qualified Sodium
import System.Exit

main = do
	args <- getArgs
	case args of
		[filename1, filename2] -> processFiles filename1 filename2
		_ -> do
			putStrLn "usage: sodium filename.pas filename.hs"
			exitWith $ ExitFailure 1

processFiles :: String -> String -> IO ()
processFiles filename1 filename2 = do
	source <- readFile filename1
	case Sodium.translate source of
		Right dest -> do
			putStrLn dest
			writeFile filename2 dest
		Left msgs -> do
			putStrLn "Can\'t translate."
			processFail msgs
			exitWith $ ExitFailure 1

processFail :: [String] -> IO ()
processFail = \case
	[] -> putStrLn "Error unknown"
	[msg] -> putStrLn $ "Error: " ++ msg
	msgs' -> do
		putStrLn "Try fixing one of the following errors:"
		void $ forM msgs' (\msg -> putStrLn $ "\t" ++ msg)
