module Sodium.Chloride.IOMagic (uncurse) where

import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program

uncurse :: Program -> (Either String) Program
uncurse (Program funcs) = do
	uncFuncs <- mapM uncurseFunc funcs
	return $ Program uncFuncs

uncurseFunc :: Func -> (Either String) Func
uncurseFunc func = do
	uncBody <- runReaderT
		(uncurseBody $ func ^. funcBody)
		(func ^. funcSig . funcParams)
	return $ funcBody .~ uncBody $ func

uncurseBody :: Body -> ReaderT Vars (Either String) Body
uncurseBody body
	= local (M.union $ body ^. bodyVars)
	$ do
		uncStatements <- mapM uncurseStatement (body ^. bodyStatements)
		return $ bodyStatements .~ uncStatements $ body

uncurseStatement :: Statement -> ReaderT Vars (Either String) Statement
uncurseStatement = \case
	Execute (ExecuteName (Name "readln")) [LValue name] -> do
		t <- lookupType name
		return $ Execute (ExecuteRead t) [LValue name]
	Execute (ExecuteName (Name "writeln")) args -> do
		args' <- forM args $ \case
			-- TODO: apply `show` to expressions
			-- as soon as typecheck is implemented
			RValue expr -> return $ RValue expr
			LValue name -> do
				t <- lookupType name
				return $ case t of
					ClString -> RValue $ Access name
					_ -> RValue $ Call (CallOperator OpShow) [Access name]
		return $ Execute ExecuteWrite args'
	ForStatement forCycle -> do
		uncBody <- uncurseBody (forCycle ^. forBody)
		return $ ForStatement (forBody .~ uncBody $ forCycle)
	MultiIfStatement multiIfBranch -> do
		let uncurseLeaf (expr, body) = do
			uncBody <- uncurseBody body
			return (expr, uncBody)
		uncLeafs <- mapM uncurseLeaf (multiIfBranch ^. multiIfLeafs )
		uncBodyElse <- uncurseBody (multiIfBranch ^. multiIfElse)
		return
			$ MultiIfStatement
			$ multiIfLeafs .~ uncLeafs
			$ multiIfElse  .~ uncBodyElse
			$ multiIfBranch
	statement -> return statement

lookupType name = do
	vars <- ask
	lift $ maybe
		(Left $ "IOMagic could not access type of " ++ show name)
		Right (M.lookup name vars)
