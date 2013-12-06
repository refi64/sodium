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
	let closure = initIndices 1 (_funcParams func)
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
		$ mapM vectorizeExpression
		$ (_bodyResults body)
	flip evalStateT indices $ do
		vecStatements <- vectorizeStatements
		vecResult <- vectorizeResult
		return $ VecBody
			(_bodyVars body)
			vecStatements
			vecResult

vectorizeArgument :: Argument -> ReaderT Indices Maybe VecArgument
vectorizeArgument = \case
	LValue name -> do
		index <- lookupIndex name
		return $ VecLValue name index
	RValue expr -> VecRValue <$> vectorizeExpression expr

vectorizeStatement :: Statement -> StateT Indices Maybe VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		vecExpr <- readerToState $ vectorizeExpression expr
		index <- readerToState $ lookupIndex name
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
		sidenames <-
			if name == Name "readln"
				then do
					let sidenames = [sidename | VecLValue sidename _ <- vecArgs]
					registerIndexUpdates sidenames
					return sidenames
				else return []
		retIndices <- readerToState $ closedIndices sidenames
		return $ VecExecute retIndices name vecArgs
	ForStatement forCycle -> do
		argIndices <- readerToState $ closedIndices (_forClosure forCycle)
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
		retIndices <- readerToState $ closedIndices (_forClosure forCycle)
		return $ VecForStatement retIndices vecForCycle

vectorizeExpression :: Expression -> ReaderT Indices Maybe VecExpression
vectorizeExpression = \case
	Primary a -> return $ VecPrimary a
	Access name -> do
		index <- lookupIndex name
		return $ VecAccess name index
	Call name exprs -> do
		vecExprs <- mapM vectorizeExpression exprs
		return $ VecCall name vecExprs

readerToState :: Monad m => ReaderT x m a -> StateT x m a
readerToState reader
	= StateT
	$ \x -> do
		a <- runReaderT reader x
		return (a, x)

lookupIndex name = do
	indices <- ask
	lift $ M.lookup name indices

registerIndexUpdates names
	= modify
	$ M.mapWithKey
	$ \name index ->
		if name `elem` names
			then succ index
			else index

closedIndices :: [Name] -> ReaderT Indices Maybe Indices
closedIndices names
	 =  M.filterWithKey
		(\name _ -> name `elem` names)
	<$> ask

initIndices :: Integer -> Vars -> Indices
initIndices n  = M.map (const n)
