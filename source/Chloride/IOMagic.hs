{-# LANGUAGE LambdaCase #-}

module Chloride.IOMagic
	( uncurse
	) where

import Control.Monad.Reader
import qualified Data.Map as M
import Chloride.Program
import Success

uncurse :: Program -> (Fail String) Program
uncurse (Program funcs) = do
	uncFuncs <- mapM uncurseFunc funcs
	return $ Program uncFuncs

uncurseFunc :: Func -> (Fail String) Func
uncurseFunc func = do
	uncBody <- runReaderT
		(uncurseBody (_funcBody func))
		(_funcParams $ _funcSig func)
	return $ func { _funcBody = uncBody }

uncurseBody :: Body -> ReaderT Vars (Fail String) Body
uncurseBody body
	= local (M.union $ _bodyVars body)
	$ do
		uncStatements <- mapM uncurseStatement (_bodyStatements body)
		return $ body { _bodyStatements = uncStatements }

uncurseStatement :: Statement -> ReaderT Vars (Fail String) Statement
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
		uncBody <- uncurseBody (_forBody forCycle)
		return $ ForStatement (forCycle { _forBody = uncBody })
	IfStatement ifBranch -> do
		uncBodyThen <- uncurseBody (_ifThen ifBranch)
		uncBodyElse <- uncurseBody (_ifElse ifBranch)
		return $ IfStatement (ifBranch {_ifThen = uncBodyThen, _ifElse = uncBodyElse})
	MultiIfStatement multiIfBranch -> do
		let uncurseLeaf (expr, body) = do
			uncBody <- uncurseBody body
			return (expr, uncBody)
		uncLeafs <- mapM uncurseLeaf (_multiIfLeafs multiIfBranch)
		uncBodyElse <- uncurseBody (_multiIfElse multiIfBranch)
		return
			$ MultiIfStatement
			$ multiIfBranch
			{ _multiIfLeafs = uncLeafs
			, _multiIfElse = uncBodyElse
			}
	statement -> return statement

lookupType name = do
	vars <- ask
	lift $ annotate (M.lookup name vars) 0
		("IOMagic could not access type of " ++ show name)
