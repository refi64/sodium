module Sodium.Chloride.Pattern (sub) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Sodium.Chloride.Program
import Sodium.SubstituteSingle

sub :: VecProgram -> VecProgram
sub = vecProgramFuncs %~ map subFunc

subFunc :: VecFunc -> VecFunc
subFunc func = maybe func id $ do
	subBody <- runReaderT
		(subBody $ func ^. vecFuncBody)
		(func ^. vecFuncSig . funcParams)
	return $ vecFuncBody .~ subBody $ func

subBody :: (Functor m, MonadPlus m) => VecBody -> ReaderT Vars m VecBody
subBody = bodyEliminateAssign <=< bodyMatchFold <=< bodyEliminateAssign

bodyEliminateAssign body
	= local (M.union $ body ^. vecBodyVars)
	$ update body <$> eliminateAssign
		(body ^. vecBodyResults)
		(body ^. vecBodyStatements)
	where update body (subResults, subStatements)
		= vecBodyResults .~ subResults
		$ vecBodyStatements .~ subStatements
		$ body

eliminateAssign
	:: (Functor m, MonadPlus m)
	=> [VecExpression]
	-> [(IndicesList, VecStatement)]
	-> ReaderT Vars m ([VecExpression], [(IndicesList, VecStatement)])
eliminateAssign bodyResults [] = return (bodyResults, [])
eliminateAssign bodyResults ((indices, statement):statements)
	= (`mplus` follow statement)
	$ case statement of
		VecAssign expr -> do
			name <- case indices of
				[name] -> return name
				_ -> mzero
			let subSingle = liftA2 (,)
				(mapM substituteSingleAccess bodyResults)
				(mapM (_2 substituteSingleAccess) statements)
			case runReaderT subSingle (name, expr) of
				SubstituteSingle subPair -> uncurry eliminateAssign subPair
				SubstituteNone   subPair -> uncurry eliminateAssign subPair
				SubstituteAmbiguous -> mzero
		statement -> (touch >=> follow) statement
	where
		follow statement
			 =  over _2 ((indices, statement):)
			<$> eliminateAssign bodyResults statements
		touch (VecForStatement forCycle)
			= VecForStatement <$> vecForBody bodyEliminateAssign forCycle
		touch (VecMultiIfStatement multiIfBranch)
			= VecMultiIfStatement <$> k multiIfBranch
			where k
				 =  vecMultiIfLeafs (mapM $ _2 bodyEliminateAssign)
				>=> vecMultiIfElse bodyEliminateAssign
		touch (VecBodyStatement body)
			= VecBodyStatement <$> bodyEliminateAssign body
		touch _ = mzero


type SubstituteAccessEnv = ((Name, Index), VecExpression)

class SubstituteSingleAccess a where
	substituteSingleAccess :: a -> ReaderT SubstituteAccessEnv SubstituteSingle a

instance SubstituteSingleAccess VecExpression where
	substituteSingleAccess = \case
		VecPrimary prim -> return $ VecPrimary prim
		VecAccess name' j -> do
			(name, expr) <- ask
			if name == (name', j)
				then lift (SubstituteSingle expr)
				else return (VecAccess name' j)
		VecCall callName exprs -> do
			VecCall callName <$> mapM substituteSingleAccess exprs
		VecFold callName exprs range -> do
			VecFold callName
				<$> mapM substituteSingleAccess exprs
				<*> substituteSingleAccess range

instance SubstituteSingleAccess VecArgument where
	substituteSingleAccess = \case
		-- This is a safe, but far not the best way to handle
		-- arguments. For now we simply forbid using the access
		-- variable in a call, but it needs to be allowed as soon
		-- as proper side effect management is implemented.
		VecLValue name' j -> do
			(name, _) <- ask
			if name == (name', j)
				then lift SubstituteAmbiguous
				else return (VecLValue name' j)
		VecRValue expr -> VecRValue <$> substituteSingleAccess expr

instance SubstituteSingleAccess VecStatement where
	substituteSingleAccess = \case
		-- It is assumed that every variable is assigned only once,
		-- since the code is vectorized. Therefore it's not the
		-- variable we are substituting, and no additional checks required.
		VecAssign expr -> VecAssign <$> substituteSingleAccess expr
		VecExecute executeName args
			 -> VecExecute executeName
			<$> mapM substituteSingleAccess args
		VecForStatement forCycle
			 -> VecForStatement
			<$> substituteSingleAccess forCycle
		VecMultiIfStatement multiIfBranch
			 -> VecMultiIfStatement
			<$> substituteSingleAccess multiIfBranch
		_ -> lift $ SubstituteAmbiguous

instance SubstituteSingleAccess VecForCycle where
	substituteSingleAccess
		 =  vecForRange substituteSingleAccess
		>=> vecForArgExprs (mapM substituteSingleAccess)
		>=> liftA2 (>>=)
			(shadowedBy . map fst . view vecForArgIndices)
			(flip subBody)
		where subBody shadowed
			| shadowed  = return
			| otherwise = vecForBody substituteSingleAccess

instance SubstituteSingleAccess VecMultiIfBranch where
	substituteSingleAccess
		 =  vecMultiIfLeafs (mapM subLeaf)
		>=> vecMultiIfElse substituteSingleAccess
		where subLeaf (expr, body) = liftA2 (,)
			(substituteSingleAccess expr)
			(substituteSingleAccess body)

instance SubstituteSingleAccess VecBody where
	substituteSingleAccess
		= liftA2 (>>=)
			(shadowedBy . M.keys . view vecBodyVars)
			(flip subBody)
		where subBody shadowed
			| shadowed = return
			| otherwise
				 =  vecBodyStatements (mapM $ _2 substituteSingleAccess)
				>=> vecBodyResults (mapM substituteSingleAccess)

shadowedBy :: Monad m => [Name] -> ReaderT SubstituteAccessEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names

bodyMatchFold body
	= local (M.union $ body ^. vecBodyVars)
	$ vecBodyStatements (mapM $ _2 statementMatchFold) body

statementMatchFold :: (Functor m, MonadPlus m) => VecStatement -> ReaderT Vars m VecStatement
statementMatchFold = \case
	VecForStatement forCycle -> VecAssign <$> forCycleMatchFold forCycle
	statement -> return statement

forCycleMatchFold
	(VecForCycle [(name1, j)] argExprs name2 range (VecBody vars [] [VecCall op args]))
	| M.null vars && args == [VecAccess name1 j, VecAccess name2 Immutable]
	= return (foldMatch op argExprs range)
forCycleMatchFold _ = mzero

foldMatch (CallOperator OpMultiply) [VecPrimary (INumber "1")] range
	= VecCall (CallOperator OpProduct) [range]
foldMatch (CallOperator OpAdd) [VecPrimary (INumber "1")] range
	= VecCall (CallOperator OpSum) [range]
foldMatch (CallOperator OpAnd) [VecPrimary BTrue] range
	= VecCall (CallOperator OpAnd') [range]
foldMatch (CallOperator OpOr) [VecPrimary BFalse] range
	= VecCall (CallOperator OpOr') [range]
foldMatch op argExprs range
	= VecFold op argExprs range
