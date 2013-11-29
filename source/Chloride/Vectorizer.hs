{-# LANGUAGE LambdaCase, TupleSections #-}

module Chloride.Vectorizer
	( vectorize
	) where

import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Chloride.Chloride

vectorize :: Func Body -> Maybe (Func VecBody)
vectorize func = do
	vecBody <- vectorizeBody
		-- TODO: wrap func name as variable
		-- in NameUnique to allow recursion
		( M.singleton (_funcName func) 0
		`M.union` initIndices (_funcParams func)
		)
		(_funcBody func)
	return $ func { _funcBody = vecBody }

vectorizeBody :: Indices -> Body -> Maybe VecBody
vectorizeBody closure body = do
	let indices = M.union
		(initIndices (_bodyVars body))
		closure
	(vecStatements, indices') <- flip runStateT indices
		$ mapM vectorizeStatement (_bodyStatements body)
	return $ VecBody
		(_bodyVars body)
		vecStatements
		indices'

vectorizeStatement :: Statement -> StateT Indices Maybe VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		indices <- get
		index <- lift $ M.lookup name indices
		vecExpr <- lift $ vectorizeExpression indices expr
		let index' = succ index
		put $ M.insert name index' indices
		return $ VecAssign name index' vecExpr
	Execute name args -> do
		indices <- get
		let vectorizeArg = \case
			LValue name -> return (VecLValue name)
			RValue expr -> VecRValue <$> vectorizeExpression indices expr
		vecArgs <- lift $ mapM vectorizeArg args
		let sidenames = [sidename | VecLValue sidename <- vecArgs]
		put
			$ flip M.mapWithKey indices
			$ \name index ->
				if name `elem` sidenames
					then succ index
					else index
		return $ VecExecute name vecArgs
	ForStatement forCycle -> do
		indices <- get
		vecFrom <- lift $ vectorizeExpression indices
			(_forFrom forCycle)
		vecTo <- lift $ vectorizeExpression indices
			(_forTo forCycle)
		let closure
			= M.fromList
			$ map (,1)
			$ _forName forCycle
			: _forClosure forCycle
		vecBody <- lift $ vectorizeBody closure (_forBody forCycle)
		put
			$ flip M.mapWithKey indices
			$ \name index ->
				if name `elem` (_forClosure forCycle)
					then succ index
					else index
		return
			$ VecForStatement
			$ VecForCycle
				(_forClosure forCycle)
				(_forName forCycle)
				vecFrom vecTo
				vecBody

vectorizeExpression :: Indices -> Expression -> Maybe VecExpression
vectorizeExpression indices = \case
	Quote  cs -> return $ VecQuote  cs
	Number cs -> return $ VecNumber cs
	Access name -> do
		index <- M.lookup name indices
		return $ VecAccess name index
	Call name exprs -> do
		vecExprs <- mapM (vectorizeExpression indices) exprs
		return $ VecCall name vecExprs
	Binary op expr1 expr2 -> do
		vecExpr1 <- vectorizeExpression indices expr1
		vecExpr2 <- vectorizeExpression indices expr2
		return $ VecBinary op vecExpr1 vecExpr2

initIndices :: Vars -> Indices
initIndices = M.map (const 0)
