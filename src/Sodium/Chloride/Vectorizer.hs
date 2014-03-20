module Sodium.Chloride.Vectorizer (vectorize) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program
import Sodium.Success

vectorize :: Program -> (Fail String) VecProgram
vectorize (Program funcs) = do
	vecFuncs <- mapM vectorizeFunc funcs
	return $ VecProgram vecFuncs

vectorizeFunc :: Func -> (Fail String) VecFunc
vectorizeFunc func = do
	let closure = initIndices 1 (func ^. funcSig . funcParams)
	(vecBodyGen, _) <- vectorizeBody closure (func ^. funcBody)
	vecBody <- vecBodyGen $ func ^. funcResults
	return $ VecFunc (func ^. funcSig) vecBody

vectorizeBody :: Indices -> Body -> (Fail String) ([Expression] -> (Fail String) VecBody, [Name])
vectorizeBody closure body = do
	let indices = M.union
		(initIndices 0 (body ^. bodyVars))
		closure
	(vecStatements, indices')
		<- flip runStateT indices
		$ mapM vectorizeStatement (body ^. bodyStatements)
	let isLocal = (`elem` M.keys (body ^.bodyVars))
	let changed
		= M.keys
		$ M.filterWithKey (\name -> (&&) (not $ isLocal name))
		$ M.intersectionWith (/=)
		indices indices'
	let vecBodyGen results = do
		vecResults
			<- flip runReaderT indices'
			$ mapM vectorizeExpression results
		return $ VecBody
			(body ^. bodyVars)
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
		vecFrom <- readerToState $ vectorizeExpression (forCycle ^. forFrom)
		vecTo <- readerToState $ vectorizeExpression (forCycle ^. forTo)
		-- TODO: wrap inner names
		-- in NameUnique to resolve name conflicts
		preIndices <- get
		-- TODO: nullify indices ??
		let closure = M.insert (forCycle ^. forName) (-1) preIndices
		(vecBodyGen, changed) <- lift $ vectorizeBody closure (forCycle ^. forBody)
		vecBody <- lift $ vecBodyGen (map Access changed)
		mapM registerIndexUpdate changed
		postIndices <- get
		argIndices <- lift $ runReaderT (closedIndices changed) preIndices
		let argExprs = map (uncurry VecAccess) argIndices
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		let vecForCycle = VecForCycle
			argIndices
			argExprs
			(forCycle ^. forName)
			vecFrom vecTo
			vecBody
		return $ VecForStatement retIndices vecForCycle
	MultiIfStatement multiIfBranch -> do
		preIndices <- get
		let vectorizeLeafGen (expr, body) = do
			vecExpr <- readerToState $ vectorizeExpression expr
			(vecBodyGen, changed) <- lift $ vectorizeBody preIndices body
			return ((vecExpr, vecBodyGen), changed)
		(vecLeafGens, changedList)
			<- unzip
			<$> mapM vectorizeLeafGen (multiIfBranch ^. multiIfLeafs )
		(vecBodyElseGen, changedElse)
			<- lift $ vectorizeBody preIndices (multiIfBranch ^. multiIfElse)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		let genLeaf (vecExpr, vecBodyGen) = do
			vecBody <- lift $ vecBodyGen accessChanged
			return (vecExpr, vecBody)
		vecLeafs <- mapM genLeaf vecLeafGens
		vecBodyElse <- lift $ vecBodyElseGen accessChanged
		let vecMultiIfBranch = VecMultiIfBranch vecLeafs vecBodyElse
		mapM registerIndexUpdate changed
		postIndices <- get
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		return $ VecMultiIfStatement retIndices vecMultiIfBranch
	BodyStatement body -> do
		preIndices <- get
		(vecBodyGen, changed) <- lift $ vectorizeBody preIndices body
		vecBody <- lift $ vecBodyGen (map Access changed)
		mapM registerIndexUpdate changed
		postIndices <- get
		retIndices <- lift $ runReaderT (closedIndices changed) postIndices
		return $ VecBodyStatement retIndices vecBody

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
