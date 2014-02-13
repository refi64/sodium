{-# LANGUAGE LambdaCase, TupleSections #-}

module Chloride.Vectorizer
	( vectorize
	) where

import Control.Monad
import Control.Applicative
import Data.List
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
	(vecBodyGen, _) <- vectorizeBody closure (_funcBody func)
	vecBody <- vecBodyGen $ _funcResults func
	return $ VecFunc (_funcSig func) vecBody

vectorizeBody :: Indices -> Body -> (Fail String) ([Expression] -> (Fail String) VecBody, [Name])
vectorizeBody closure body = do
	let indices = M.union
		(initIndices 0 (_bodyVars body))
		closure
	(vecStatements, indices')
		<- flip runStateT indices
		$ mapM vectorizeStatement
			(_bodyStatements body)
	let changed
		= M.keys
		$ M.filter id
		$ M.intersectionWith (/=)
		indices indices'
	let vecBodyGen results = do
		vecResults
			<- flip runReaderT indices'
			$ mapM vectorizeExpression
				results
		return $ VecBody
			(_bodyVars body)
			vecStatements
			vecResults
	return (vecBodyGen, changed)

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
		vecFrom <- readerToState $ vectorizeExpression (_forFrom forCycle)
		vecTo <- readerToState $ vectorizeExpression (_forTo forCycle)
		-- TODO: wrap inner names
		-- in NameUnique to resolve name conflicts
		preIndices <- get
		let closure
			= M.insert (_forName forCycle) (-1)
			$ preIndices
		(vecBodyGen, changed) <- lift $ vectorizeBody closure (_forBody forCycle)
		vecBody <- lift $ vecBodyGen (map Access changed)
		mapM registerIndexUpdate changed
		postIndices <- get
		argIndices <- lift $ runReaderT (closedIndices changed) preIndices
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		let vecForCycle = VecForCycle
			argIndices
			(_forName forCycle)
			vecFrom vecTo
			vecBody
		return $ VecForStatement retIndices vecForCycle
	IfStatement ifBranch -> do
		vecExpr <- readerToState $ vectorizeExpression (_ifExpr ifBranch)
		preIndices <- get
		(vecBodyThenGen, changedThen) <- lift $ vectorizeBody
			preIndices
			(_ifThen ifBranch)
		(vecBodyElseGen, changedElse) <- lift $ vectorizeBody
			preIndices
			(_ifElse ifBranch)
		let changed = nub $ changedThen ++ changedElse
		let accessChanged = map Access changed
		vecBodyThen <- lift $ vecBodyThenGen accessChanged
		vecBodyElse <- lift $ vecBodyElseGen accessChanged
		let vecIfBranch = VecIfBranch
			vecExpr
			vecBodyThen
			vecBodyElse
		mapM registerIndexUpdate changed
		postIndices <- get
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		return $ VecIfStatement retIndices vecIfBranch
	CaseStatement caseBranch -> do
		vecExpr <- readerToState $ vectorizeExpression (_caseExpr caseBranch)
		preIndices <- get
		let vectorizeLeafGen (exprs, body) = do
			vecExprs <- readerToState $ mapM vectorizeExpression exprs
			(vecBodyGen, changed) <- lift $ vectorizeBody
				preIndices
				body
			return ((vecExprs, vecBodyGen), changed)
		(vecLeafGens, changedList)
			<-  unzip
			<$> mapM vectorizeLeafGen (_caseLeafs caseBranch)
		(vecBodyElseGen, changedElse)
			<- lift $ vectorizeBody preIndices (_caseElse caseBranch)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		let genLeaf (exprs, leafGen) = do
			leafBody <- lift $ leafGen accessChanged
			return (exprs, leafBody)
		vecLeafs <- mapM genLeaf vecLeafGens
		vecBodyElse <- lift $ vecBodyElseGen accessChanged
		let vecCaseBranch = VecCaseBranch vecExpr vecLeafs vecBodyElse
		mapM registerIndexUpdate changed
		postIndices <- get
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		return $ VecCaseStatement retIndices vecCaseBranch

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
	let indexUpdate index
		| index < 0 = annotate Nothing 0
			("Vectorizer could not update an immutable value " ++ show name)
		| otherwise = return (succ index)
	index <- readerToState $ lookupIndex name
	index' <- lift $ indexUpdate index
	modify $ M.insert name index'
	return index'

closedIndices :: [Name] -> ReaderT Indices (Fail String) IndicesList
closedIndices = mapM lookupIndex' where
	lookupIndex' name = do
		index <- lookupIndex name
		return (name, index)

initIndices :: Integer -> Vars -> Indices
initIndices n  = M.map (const n)
