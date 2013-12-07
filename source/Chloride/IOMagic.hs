{-# LANGUAGE LambdaCase #-}

module Chloride.IOMagic
	( uncurse
	) where

import Control.Monad.Reader
import qualified Data.Map as M
import Chloride.Chloride

uncurse :: Program -> Maybe Program
uncurse (Program funcs) = do
	uncFuncs <- mapM uncurseFunc funcs
	return $ Program uncFuncs

uncurseFunc :: Func Body -> Maybe (Func Body)
uncurseFunc func = do
	uncBody <- runReaderT
		(uncurseBody (_funcBody func))
		(_funcParams func)
	return $ func { _funcBody = uncBody }

uncurseBody :: Body -> ReaderT Vars Maybe Body
uncurseBody body
	= local (M.union $ _bodyVars body)
	$ do
		uncStatements <- mapM uncurseStatement (_bodyStatements body)
		return $ body { _bodyStatements = uncStatements }

uncurseStatement :: Statement -> ReaderT Vars Maybe Statement
uncurseStatement = \case
	Execute (ExecuteName (Name "readln")) args ->
		return $ Execute ExecuteRead args
	Execute (ExecuteName (Name "writeln")) args -> do
		-- TODO: handle types and apply `show`
		return $ Execute ExecuteWrite args
	ForStatement forCycle -> do
		uncBody <- uncurseBody (_forBody forCycle)
		return $ ForStatement (forCycle { _forBody = uncBody })
	statement -> return statement
