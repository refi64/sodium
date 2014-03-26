module Sodium.Chloride.Pattern (sub) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens
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
subBody = bodyEliminateAssign

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
	-> [VecStatement]
	-> ReaderT Vars m ([VecExpression], [VecStatement])
eliminateAssign bodyResults [] = return (bodyResults, [])
eliminateAssign bodyResults (statement:statements) = msum
	[ case statement of
		VecAssign name i expr -> do
			let subSingle = liftA2 (,)
				(mapM substituteSingleAccess bodyResults)
				(mapM substituteSingleAccess statements)
			case runReaderT subSingle ((name, i), expr) of
				SubstituteSingle subPair -> uncurry eliminateAssign subPair
				SubstituteNone   subPair -> uncurry eliminateAssign subPair
				SubstituteAmbiguous -> mzero
		VecForStatement indices forCycle -> do
			subBody <- bodyEliminateAssign (forCycle ^. vecForBody)
			let statement = VecForStatement indices
				(vecForBody .~ subBody $ forCycle)
			over _2 (statement:) <$> eliminateAssign bodyResults statements
		_ -> mzero
	, over _2 (statement:) <$> eliminateAssign bodyResults statements
	]


type SubstituteAccessEnv = ((Name, Integer), VecExpression)

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
		VecAssign name i expr
			 -> VecAssign name i
			<$> substituteSingleAccess expr
		VecExecute indices executeName args
			 -> VecExecute indices executeName
			<$> mapM substituteSingleAccess args
		VecForStatement indices forCycle
			 -> VecForStatement indices
			<$> substituteSingleAccess forCycle
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

instance SubstituteSingleAccess VecBody where
	substituteSingleAccess
		= liftA2 (>>=)
			(shadowedBy . M.keys . view vecBodyVars)
			(flip subBody)
		where subBody shadowed
			| shadowed = return
			| otherwise
				 =  vecBodyStatements (mapM substituteSingleAccess)
				>=> vecBodyResults (mapM substituteSingleAccess)

shadowedBy :: Monad m => [Name] -> ReaderT SubstituteAccessEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names
