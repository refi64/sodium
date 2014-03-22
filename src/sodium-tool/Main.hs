module Main (main) where

import System.Environment
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
		Left msg -> do
			putStrLn msg
			exitWith $ ExitFailure 1
