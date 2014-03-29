{-# LANGUAGE DeriveDataTypeable #-}
module Sodium.Chloride.Vectorizer (vectorize) where

import Data.List
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Chloride.Program
import Control.Exception
import Data.Typeable

data VectorizerException
	= NoAccess Name
	| UpdateImmutable Name
	deriving (Show, Typeable)

instance Exception VectorizerException

vectorize :: Program -> VecProgram
vectorize
	= VecProgram
	. map vectorizeFunc
	. view programFuncs

vectorizeFunc :: Func -> VecFunc
vectorizeFunc func = VecFunc (func ^. funcSig) $ vecBodyGen (func ^. funcResults) where
	closure = initIndices (Index 0) (func ^. funcSig . funcParams)
	(vecBodyGen, _) = vectorizeBody closure (func ^. funcBody)

vectorizeBody :: Indices -> Body -> ([Expression] -> VecBody, [Name])
vectorizeBody closure body = (vecBodyGen, changed) where
	isLocal = flip elem $ M.keys (body ^.bodyVars)
	indices = initIndices Uninitialized (body ^. bodyVars) `M.union` closure
	(vecStatements, indices')
		= flip runState indices
		$ mapM vectorizeStatement (body ^. bodyStatements)
	changed
		= M.keys
		$ M.filterWithKey ((&&) . not . isLocal)
		$ M.intersectionWith (/=)
		indices indices'
	vecBodyGen results
		= VecBody (body ^. bodyVars) vecStatements
		$ runReader (mapM vectorizeExpression results) indices'

vectorizeBody' :: Indices -> Body -> (VecBody, [Name])
vectorizeBody' closure body = (vecBody, changed) where
	(vecBodyGen, changed) = vectorizeBody closure body
	vecBody = vecBodyGen (map Access changed)

vectorizeArgument :: Argument -> Reader Indices VecArgument
vectorizeArgument = \case
	LValue name -> VecLValue name <$> lookupIndex name
	RValue expr -> VecRValue <$> vectorizeExpression expr

vectorizeStatement :: Statement -> State Indices VecStatement
vectorizeStatement = \case
	Assign name expr -> do
		vecExpr <- readerToState (vectorizeExpression expr)
		retIndices <- mapM registerIndexUpdate [name]
		return $ VecAssign retIndices vecExpr
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
		vecRange <- readerToState $ vectorizeExpression (forCycle ^. forRange)
		preIndices <- get
		let closure = M.insert (forCycle ^. forName) Immutable preIndices
		let (vecBody, changed) = vectorizeBody' closure (forCycle ^. forBody)
		let argIndices = runReader (closedIndices changed) preIndices
		retIndices <- mapM registerIndexUpdate changed
		return
			$ VecForStatement retIndices
			$ VecForCycle
				argIndices
				(uncurry VecAccess `map` argIndices)
				(forCycle ^. forName)
				vecRange
				vecBody
	MultiIfStatement multiIfBranch -> do
		preIndices <- get
		(vecLeafGens, changedList) <- (unzip <$>)
			$ forM (multiIfBranch ^. multiIfLeafs)
			$ \(expr, body)
				 -> over _1 . (,)
				<$> readerToState (vectorizeExpression expr)
				<*> return (vectorizeBody preIndices body)
		let (vecBodyElseGen, changedElse)
			= vectorizeBody preIndices (multiIfBranch ^. multiIfElse)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		let vecMultiIfBranch = VecMultiIfBranch
			(over _2 ($ accessChanged) `map` vecLeafGens)
			(vecBodyElseGen accessChanged)
		retIndices <- mapM registerIndexUpdate changed
		return $ VecMultiIfStatement retIndices vecMultiIfBranch
	BodyStatement body -> do
		preIndices <- get
		let (vecBody, changed) = vectorizeBody' preIndices body
		retIndices <- mapM registerIndexUpdate changed
		return $ VecBodyStatement retIndices vecBody

vectorizeExpression :: Expression -> Reader Indices VecExpression
vectorizeExpression = \case
	Primary a -> return (VecPrimary a)
	Access name -> VecAccess name <$> lookupIndex name
	Call name exprs -> VecCall name <$> mapM vectorizeExpression exprs

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex name
	 =  maybe (throw $ NoAccess name) id
	<$> (M.lookup name <$> ask)

registerIndexUpdate name = do
	index <- readerToState $ lookupIndex name
	let index' = indexUpdate index
	modify $ M.insert name index'
	return (name, index')
	where indexUpdate = \case
		Index n -> Index (succ n)
		Uninitialized -> Index 0
		Immutable -> throw $ UpdateImmutable name

closedIndices :: [Name] -> Reader Indices IndicesList
closedIndices = mapM $ \name -> (name,) <$> lookupIndex name

initIndices :: Index -> Vars -> Indices
initIndices n = M.map (const n)
