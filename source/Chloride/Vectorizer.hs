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
import Success

vectorize :: Program -> (Fail String) VecProgram
vectorize (Program funcs) = do
	vecFuncs <- mapM vectorizeFunc funcs
	return $ VecProgram vecFuncs

vectorizeFunc :: Func -> (Fail String) VecFunc
vectorizeFunc func = do
	let closure = initIndices 1 (_funcParams $ _funcSig func)
	vecBody <- vectorizeBody closure (_funcBody func) (_funcResults func)
	return $ VecFunc (_funcSig func) vecBody

vectorizeBody :: Indices -> Body -> [Expression] -> (Fail String) VecBody
vectorizeBody closure body results = do
	let indices = M.union
		(initIndices 0 (_bodyVars body))
		closure
	let vectorizeStatements = mapM
		vectorizeStatement
		(_bodyStatements body)
	let vectorizeResult
		= readerToState
		$ mapM vectorizeExpression
		$ results
	flip evalStateT indices $ do
		vecStatements <- vectorizeStatements
		vecResult <- vectorizeResult
		return $ VecBody
			(_bodyVars body)
			vecStatements
			vecResult

vectorizeArgument :: Argument -> ReaderT Indices (Fail String) VecArgument
vectorizeArgument = \case
	LValue name -> do
		index <- lookupIndex name
		return $ VecLValue name index
	RValue expr -> VecRValue <$> vectorizeExpression expr

vectorizeStatement :: Statement -> StateT Indices (Fail String) VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		vecExpr <- readerToState $ vectorizeExpression expr
		index <- registerIndexUpdate name
		return $ VecAssign name index vecExpr
	Execute name args -> do
		vecArgs <- readerToState $ mapM vectorizeArgument args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		-- HACK: for now we check if the procedure
		-- is ReadLn, because only ReadLn is allowed
		-- to change its LValues
		sidenames <- case name of
			ExecuteRead _ -> do
					let sidenames = [sidename | VecLValue sidename _ <- vecArgs]
					mapM registerIndexUpdate sidenames
					return sidenames
			_ -> return []
		retIndices <- readerToState $ closedIndices sidenames
		return $ VecExecute retIndices name vecArgs
	ForStatement forCycle -> do
		argIndices <- readerToState $ closedIndices (_forClosure forCycle)
		vecFrom <- readerToState $ vectorizeExpression (_forFrom forCycle)
		vecTo <- readerToState $ vectorizeExpression (_forTo forCycle)
		-- TODO: wrap inner names
		-- in NameUnique to resolve name conflicts
		let closure'
			= M.insert (_forName forCycle) (-1)
			$ M.fromList (map (,1) $ _forClosure forCycle)
		closure <- M.union closure' <$> get
		vecBody <- lift $ vectorizeBody closure
			(_forBody forCycle)
			(map Access $ _forClosure forCycle)
		mapM registerIndexUpdate (_forClosure forCycle)
		let vecForCycle = VecForCycle
			argIndices
			(_forName forCycle)
			vecFrom vecTo
			vecBody
		retIndices <- readerToState $ closedIndices (_forClosure forCycle)
		return $ VecForStatement retIndices vecForCycle
	IfStatement ifBranch -> do
		vecExpr <- readerToState $ vectorizeExpression (_ifExpr ifBranch)
		closure <- readerToState $ closedIndices (_ifClosure ifBranch)
		vecBodyThen <- lift $ vectorizeBody
			(M.fromList closure)
			(_ifThen ifBranch)
			(map Access $ _ifClosure ifBranch)
		vecBodyElse <- lift $ vectorizeBody
			(M.fromList closure)
			(_ifElse ifBranch)
			(map Access $ _ifClosure ifBranch)
		mapM registerIndexUpdate (_ifClosure ifBranch)
		let vecIfBranch = VecIfBranch
			vecExpr
			vecBodyThen
			vecBodyElse
		retIndices <- readerToState $ closedIndices (_ifClosure ifBranch)
		return $ VecIfStatement retIndices vecIfBranch

vectorizeExpression :: Expression -> ReaderT Indices (Fail String) VecExpression
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
	lift $ annotate (M.lookup name indices) 0
		("Vectorizer could not access index of " ++ show name)

registerIndexUpdate name = do
	index <- readerToState $ lookupIndex name
	index' <- if index < 0
		then lift (Fail 0 ["Vectorizer could not update an immutable value " ++ show name])
		else return (succ index)
	modify $ M.insert name index'
	return index'

closedIndices :: [Name] -> ReaderT Indices (Fail String) IndicesList
closedIndices = mapM lookupIndex' where
	lookupIndex' name = do
		index <- lookupIndex name
		return (name, index)

initIndices :: Integer -> Vars -> Indices
initIndices n  = M.map (const n)
