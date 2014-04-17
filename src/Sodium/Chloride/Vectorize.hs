{-# LANGUAGE DeriveDataTypeable #-}
module Sodium.Chloride.Vectorize (vectorize) where

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
vectorizeFunc func
	= VecFunc (func ^. funcSig)
	$ vecBodyGen (func ^. funcResults)
	where (_, vecBodyGen)
		= runReader (vectorizeBody (func ^. funcBody))
		$ initIndices (Index 0) (func ^. funcSig . funcParams)

vectorizeBody :: Body -> Reader Indices ([Name], [Expression] -> VecBody)
vectorizeBody body = reader (vectorizeBodyR body)

vectorizeBodyR body closure = (changed, vecBodyGen) where
	isLocal = flip elem $ M.keys (body ^.bodyVars)
	indices = initIndices Uninitialized (body ^. bodyVars) `M.union` closure
	vectorizeStatement' statement
		= _1 (mapM registerIndexUpdate)
		=<< readerToState (vectorizeStatement statement)
	(vecStatements, indices')
		= flip runState indices
		$ mapM vectorizeStatement' (body ^. bodyStatements)
	changed
		= M.keys
		$ M.filterWithKey ((&&) . not . isLocal)
		$ M.intersectionWith (/=)
		indices indices'
	vecBodyGen results
		= VecBody (body ^. bodyVars) vecStatements
		$ runReader (mapM vectorizeExpression results) indices'

vectorizeBody' :: Body -> Reader Indices ([Name], VecBody)
vectorizeBody' body = do
	(changed, vecBodyGen) <- vectorizeBody body
	let vecBody = vecBodyGen (map Access changed)
	return (changed, vecBody)

vectorizeStatement :: Statement -> Reader Indices ([Name], VecStatement)
vectorizeStatement = \case
	BodyStatement body
		 -> over _2 VecBodyStatement
		<$> vectorizeBody' body
	Assign name expr
		 -> (,) [name]
		<$> (VecAssign <$> vectorizeExpression expr)
	Execute mres name args -> do
		vecArgs <- mapM vectorizeExpression args
		-- TODO: typecheck in order to find out
		-- what lvalues can actually get changed
		let sidenames = []
		let resnames = maybe empty pure mres
		return $ (resnames ++ sidenames, VecExecute name vecArgs)
	ForStatement forCycle -> over _2 VecForStatement <$> do
		vecRange <- vectorizeExpression (forCycle ^. forRange)
		(changed, vecBody)
			<- local (M.insert (forCycle ^. forName) Immutable)
			 $ vectorizeBody' (forCycle ^. forBody)
		argIndices <- closedIndices changed
		let vecForCycle = VecForCycle
			argIndices
			(uncurry VecAccess `map` argIndices)
			(forCycle ^. forName)
			vecRange
			vecBody
		return (changed, vecForCycle)
	MultiIfStatement multiIfBranch -> over _2 VecMultiIfStatement <$> do
		(changedList, vecLeafGens) <- (unzip <$>)
			$ forM (multiIfBranch ^. multiIfLeafs)
			$ \(expr, body)
				 -> over _2 . (,)
				<$> vectorizeExpression expr
				<*> vectorizeBody body
		(changedElse, vecBodyElseGen)
			<- vectorizeBody (multiIfBranch ^. multiIfElse)
		let changed = nub $ changedElse ++ concat changedList
		let accessChanged = map Access changed
		let vecMultiIfBranch = VecMultiIfBranch
			(over _2 ($ accessChanged) `map` vecLeafGens)
			(vecBodyElseGen accessChanged)
		return $ (changed, vecMultiIfBranch)

vectorizeExpression :: Expression -> Reader Indices VecExpression
vectorizeExpression = \case
	Primary a -> return (VecPrimary a)
	Access name -> VecAccess name <$> lookupIndex name
	Call name exprs -> VecCall name <$> mapM vectorizeExpression exprs

readerToState :: (Functor m, Monad m) => ReaderT x m a -> StateT x m a
readerToState reader = StateT $ \x -> (,x) <$> runReaderT reader x

lookupIndex name
	 =  maybe (throw $ NoAccess name) id
	<$> reader (M.lookup name)

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
