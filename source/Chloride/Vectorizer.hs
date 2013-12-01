{-# LANGUAGE LambdaCase, TupleSections #-}

module Chloride.Vectorizer
	( vectorize
	) where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Chloride.Chloride

vectorize :: Func Body -> Maybe (Func VecBody)
vectorize func = do
	let closure = M.union
			(M.singleton (_funcRetName func) 0)
			(initIndices (_funcParams func))
	vecBody <- vectorizeBody closure (_funcBody func)
	return $ func { _funcBody = vecBody }

vectorizeBody :: Indices -> Body -> Maybe VecBody
vectorizeBody closure body = do
	let indices = M.union
		(initIndices (_bodyVars body))
		closure
	let vectorizeStatements = mapM
		vectorizeStatement
		(_bodyStatements body)
	(vecStatements, indices') <- runStateT
		vectorizeStatements
		indices
	return $ VecBody
		(_bodyVars body)
		vecStatements
		indices'

vectorizeStatement :: Statement -> StateT Indices Maybe VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		vecExpr <- readerToState $ vectorizeExpression expr
		index <- do
			indicies <- get
			lift $ M.lookup name indicies
		let index' = succ index
		modify $ M.insert name index'
		return $ VecAssign name index' vecExpr
	Execute name args -> do
		let vectorizeArg = \case
			LValue name -> LValue <$> return name
			RValue expr -> RValue <$> vectorizeExpression expr
		vecArgs <- readerToState $ mapM vectorizeArg args
		registerIndexUpdates [sidename | LValue sidename <- vecArgs]
		return $ VecExecute name vecArgs
	ForStatement forCycle -> do
		vecFrom <- readerToState $ vectorizeExpression (_forFrom forCycle)
		vecTo <- readerToState $ vectorizeExpression (_forTo forCycle)
		-- TODO: wrap inner names
		-- in NameUnique to resolve name conflicts
		let closure
			= M.fromList
			$ map (,1)
			$ _forName forCycle
			: _forClosure forCycle
		vecBody <- lift $ vectorizeBody closure (_forBody forCycle)
		registerIndexUpdates (_forClosure forCycle)
		return
			$ VecForStatement
			$ VecForCycle
				(_forClosure forCycle)
				(_forName forCycle)
				vecFrom vecTo
				vecBody

vectorizeExpression :: Expression -> ReaderT Indices Maybe VecExpression
vectorizeExpression = \case
	Primary a -> return $ VecPrimary a
	Access name -> do
		index <- do
			indicies <- ask
			lift $ M.lookup name indicies
		return $ VecAccess name index
	Call name exprs -> do
		vecExprs <- mapM vectorizeExpression exprs
		return $ VecCall name vecExprs
	Binary op expr1 expr2 -> do
		vecExpr1 <- vectorizeExpression expr1
		vecExpr2 <- vectorizeExpression expr2
		return $ VecBinary op vecExpr1 vecExpr2

readerToState :: Monad m => ReaderT x m a -> StateT x m a
readerToState reader
	= StateT
	$ \x -> do
		a <- runReaderT reader x
		return (a, x)

registerIndexUpdates names
	= modify
	$ M.mapWithKey
	$ \name index ->
		if name `elem` names
			then succ index
			else index

initIndices :: Vars -> Indices
initIndices = M.map (const 0)
