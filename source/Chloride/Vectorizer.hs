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
		`M.union` initIndicies (_funcParams func)
		)
		(_funcBody func)
	return $ func { _funcBody = vecBody }

vectorizeBody :: Indicies -> Body -> Maybe VecBody
vectorizeBody closure body = do
	let indicies = M.union
		(initIndicies (_bodyVars body))
		closure
	(vecStatements, indicies') <- flip runStateT indicies
		$ mapM vectorizeStatement (_bodyStatements body)
	return $ VecBody
		(_bodyVars body)
		vecStatements
		indicies'

vectorizeStatement :: Statement -> StateT Indicies Maybe VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		indicies <- get
		index <- lift $ M.lookup name indicies
		vecExpr <- lift $ vectorizeExpression indicies expr
		let index' = succ index
		put $ M.insert name index' indicies
		return $ VecAssign name index' vecExpr
	Execute name args -> do
		indicies <- get
		let vectorizeArg = \case
			LValue name -> return (VecLValue name)
			RValue expr -> VecRValue <$> vectorizeExpression indicies expr
		vecArgs <- lift $ mapM vectorizeArg args
		let sidenames = [sidename | VecLValue sidename <- vecArgs]
		put
			$ flip M.mapWithKey indicies
			$ \name index ->
				if name `elem` sidenames
					then succ index
					else index
		return $ VecExecute name vecArgs
	ForStatement forCycle -> do
		indicies <- get
		vecFrom <- lift $ vectorizeExpression indicies
			(_forFrom forCycle)
		vecTo <- lift $ vectorizeExpression indicies
			(_forTo forCycle)
		let closure
			= M.fromList
			$ map (,1)
			$ _forName forCycle
			: _forClosure forCycle
		vecBody <- lift $ vectorizeBody closure (_forBody forCycle)
		put
			$ flip M.mapWithKey indicies
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

vectorizeExpression :: Indicies -> Expression -> Maybe VecExpression
vectorizeExpression indicies = \case
	Quote  cs -> return $ VecQuote  cs
	Number cs -> return $ VecNumber cs
	Access name -> do
		index <- M.lookup name indicies
		return $ VecAccess name index
	Call name exprs -> do
		vecExprs <- mapM (vectorizeExpression indicies) exprs
		return $ VecCall name vecExprs
	Binary op expr1 expr2 -> do
		vecExpr1 <- vectorizeExpression indicies expr1
		vecExpr2 <- vectorizeExpression indicies expr2
		return $ VecBinary op vecExpr1 vecExpr2

initIndicies :: Vars -> Indicies
initIndicies = M.map (const 0)
