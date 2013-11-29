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
	(indicies', vecStatements) <- foldM
		vectorizeStatement
		(indicies, [])
		(_bodyStatements body)
	return $ VecBody
		(_bodyVars body)
		vecStatements
		indicies'

vectorizeStatement
	:: (Indicies, [VecStatement])
	-> Statement
	-> Maybe (Indicies, [VecStatement])
vectorizeStatement (indicies, vecStatements) = \case
	Assign name expr -> do
		index <- M.lookup name indicies
		vecExpr <- vectorizeExpression indicies expr
		let index' = succ index
		let indicies' = M.insert name index' indicies
		let vecStatement = VecAssign name index' vecExpr
		return (indicies', vecStatement:vecStatements)
	Execute name args -> do
		let vectorizeArg = \case
			LValue name -> return (VecLValue name)
			RValue expr -> VecRValue <$> vectorizeExpression indicies expr
		vecArgs <- mapM vectorizeArg args
		let sidenames = [sidename | VecLValue sidename <- vecArgs]
		let indicies'
			= flip M.mapWithKey indicies
			$ \name index ->
				if name `elem` sidenames
					then succ index
					else index
		let vecStatement = VecExecute name vecArgs
		return (indicies', vecStatement:vecStatements)
	ForStatement forCycle -> do
		let closure
			= M.fromList
			$ map (,1)
			$ _forName forCycle
			: _forClosure forCycle
		vecFrom <- vectorizeExpression indicies
			(_forFrom forCycle)
		vecTo <- vectorizeExpression indicies
			(_forTo forCycle)
		vecBody <- vectorizeBody closure (_forBody forCycle)
		let vecStatement = VecForStatement $ VecForCycle
			(_forClosure forCycle)
			(_forName forCycle)
			vecFrom
			vecTo
			vecBody
		let indicies'
			= flip M.mapWithKey indicies
			$ \name index ->
				if name `elem` (_forClosure forCycle)
					then succ index
					else index
		return (indicies', vecStatement:vecStatements)

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
