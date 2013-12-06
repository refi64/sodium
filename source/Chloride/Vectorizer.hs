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
			(initIndices 1 (_funcParams func))
	vecBody <- vectorizeBody closure (_funcBody func)
	return $ func { _funcBody = vecBody }

vectorizeBody :: Indices -> Body -> Maybe VecBody
vectorizeBody closure body = do
	let indices = M.union
		(initIndices 0 (_bodyVars body))
		closure
	let vectorizeStatements = mapM
		vectorizeStatement
		(_bodyStatements body)
	let vectorizeResult
		= readerToState
		$ vectorizeExpression
		$ (_bodyResult body)
	((vecStatements, vecResult), indices') <- runStateT
		((,) <$> vectorizeStatements <*> vectorizeResult)
		indices
	return $ VecBody
		(_bodyVars body)
		vecStatements
		-- Not sure if this filter is a hack...
		(M.filter (>0) indices')
		vecResult

vectorizeArgument :: Argument -> ReaderT Indices Maybe VecArgument
vectorizeArgument = \case
	LValue name -> do
		index <- do
			indices <- ask
			lift $ M.lookup name indices
		return $ VecLValue name index
	RValue expr -> VecRValue <$> vectorizeExpression expr

vectorizeStatement :: Statement -> StateT Indices Maybe VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		vecExpr <- readerToState $ vectorizeExpression expr
		index <- do
			indices <- get
			lift $ M.lookup name indices
		let index' = succ index
		modify $ M.insert name index'
		return $ VecAssign name index' vecExpr
	Execute name args -> do
		vecArgs <- readerToState $ mapM vectorizeArgument args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		-- HACK: for now we check if the procedure
		-- is ReadLn, because only ReadLn is allowed
		-- to change its LValues
		if name == Name "readln"
			then registerIndexUpdates [sidename | VecLValue sidename _ <- vecArgs]
			else return ()
		return $ VecExecute name vecArgs
	ForStatement forCycle -> do
		argIndices <- do
			indices <- get
			return $ M.filterWithKey
				(\name _ -> name `elem` (_forClosure forCycle))
				indices
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
		let vecForCycle = VecForCycle
			argIndices
			(_forName forCycle)
			vecFrom vecTo
			vecBody
		retIndices <- do
			indices <- get
			return $ M.filterWithKey
				(\name _ -> name `elem` (_forClosure forCycle))
				indices
		return $ VecForStatement retIndices vecForCycle

vectorizeExpression :: Expression -> ReaderT Indices Maybe VecExpression
vectorizeExpression = \case
	Primary a -> return $ VecPrimary a
	Access name -> do
		index <- do
			indices <- ask
			lift $ M.lookup name indices
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

initIndices :: Integer -> Vars -> Indices
initIndices n  = M.map (const n)
