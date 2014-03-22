module Sodium.Chloride.Vectorizer (vectorize) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program

vectorize :: Program -> Either String VecProgram
vectorize (Program funcs) = VecProgram <$> mapM vectorizeFunc funcs

vectorizeFunc :: Func -> Either String VecFunc
vectorizeFunc func = do
	let closure = initIndices 1 (func ^. funcSig . funcParams)
	(vecBodyGen, _) <- vectorizeBody closure (func ^. funcBody)
	VecFunc (func ^. funcSig) <$> vecBodyGen (func ^. funcResults)

vectorizeBody :: Indices -> Body -> (Either String) ([Expression] -> (Either String) VecBody, [Name])
vectorizeBody closure body = do
	let isLocal = flip elem $ M.keys (body ^.bodyVars)
	let indices = initIndices 0 (body ^. bodyVars) `M.union` closure
	(vecStatements, indices')
		<- flip runStateT indices
		$ mapM vectorizeStatement (body ^. bodyStatements)
	let changed
		= M.keys
		$ M.filterWithKey ((&&) . not . isLocal)
		$ M.intersectionWith (/=)
		indices indices'
	let vecBodyGen results
		 =  VecBody (body ^. bodyVars)
		<$> return vecStatements
		<*> flip runReaderT indices' (mapM vectorizeExpression results)
	return (vecBodyGen, changed)

vectorizeBody' :: Indices -> Body -> Either String (VecBody, [Name])
vectorizeBody' closure body = do
	(vecBodyGen, changed) <- vectorizeBody closure body
	vecBody <- vecBodyGen (map Access changed)
	return (vecBody, changed)

vectorizeArgument :: Argument -> ReaderT Indices (Either String) VecArgument
vectorizeArgument = \case
	LValue name -> VecLValue name <$> lookupIndex name
	RValue expr -> VecRValue <$> vectorizeExpression expr

vectorizeStatement :: Statement -> StateT Indices (Either String) VecStatement
vectorizeStatement = \case
	Assign name expr
		 -> flip (uncurry VecAssign)
		<$> readerToState (vectorizeExpression expr)
		<*> registerIndexUpdate name
	Execute name args -> do
		vecArgs <- readerToState $ mapM vectorizeArgument args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		-- HACK: for now we check if the procedure
		-- is ReadLn, because only ReadLn is allowed
		-- to change its LValues
		retIndices <- case name of
			ExecuteRead _ -> do
					let sidenames = [sidename | VecLValue sidename _ <- vecArgs]
					mapM registerIndexUpdate sidenames
			_ -> return []
		return $ VecExecute retIndices name vecArgs
	ForStatement forCycle -> do
		(vecFrom, vecTo) <- readerToState
			 $  (,)
			<$> vectorizeExpression (forCycle ^. forFrom)
			<*> vectorizeExpression (forCycle ^. forTo)
		preIndices <- get
		let closure = M.insert (forCycle ^. forName) (-1) preIndices
		(vecBody, changed) <- lift $ vectorizeBody' closure (forCycle ^. forBody)
		argIndices <- lift $ runReaderT (closedIndices changed) preIndices
		retIndices <- mapM registerIndexUpdate changed
		return
			$ VecForStatement retIndices
			$ VecForCycle
				argIndices
				(uncurry VecAccess `map` argIndices)
				(forCycle ^. forName)
				vecFrom vecTo
				vecBody
	MultiIfStatement multiIfBranch -> do
		preIndices <- get
		(vecLeafGens, changedList) <- (unzip <$>)
			$ forM (multiIfBranch ^. multiIfLeafs)
			$ \(expr, body)
				 -> over _1 . (,)
				<$> readerToState (vectorizeExpression expr)
				<*> lift (vectorizeBody preIndices body)
		(vecBodyElseGen, changedElse) <- lift
			$ vectorizeBody preIndices (multiIfBranch ^. multiIfElse)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		vecMultiIfBranch <- lift
			 $  VecMultiIfBranch
			<$> forM vecLeafGens (_2 $ flip ($) accessChanged)
			<*> vecBodyElseGen accessChanged
		retIndices <- mapM registerIndexUpdate changed
		return $ VecMultiIfStatement retIndices vecMultiIfBranch
	BodyStatement body -> do
		preIndices <- get
		(vecBody, changed) <- lift $ vectorizeBody' preIndices body
		retIndices <- mapM registerIndexUpdate changed
		return $ VecBodyStatement retIndices vecBody

vectorizeExpression :: Expression -> ReaderT Indices (Either String) VecExpression
vectorizeExpression = \case
	Primary a -> return (VecPrimary a)
	Access name -> VecAccess name <$> lookupIndex name
	Call name exprs -> VecCall name <$> mapM vectorizeExpression exprs

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex name = do
	indices <- ask
	lift $ maybe
		(Left $ "Vectorizer could not access index of " ++ show name)
		Right (M.lookup name indices)
registerIndexUpdate name = do
	let indexUpdate index
		| index < 0 = Left
			$ "Vectorizer could not update an immutable value " ++ show name
		| otherwise = return (succ index)
	index <- readerToState $ lookupIndex name
	index' <- lift $ indexUpdate index
	modify $ M.insert name index'
	return (name, index')

closedIndices :: [Name] -> ReaderT Indices (Either String) IndicesList
closedIndices = mapM $ \name -> (name,) <$> lookupIndex name

initIndices :: Integer -> Vars -> Indices
initIndices n  = M.map (const n)
