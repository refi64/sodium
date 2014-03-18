module Sodium.Chloride.Pattern (sub) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program
import Sodium.Success

sub :: VecProgram -> (Fail String) VecProgram
sub (VecProgram funcs) = do
	subFuncs <- mapM subFunc funcs
	return $ VecProgram subFuncs

subFunc :: VecFunc -> (Fail String) VecFunc
subFunc func = do
	subBody <- runReaderT
		(subBody (func ^. vecFuncBody))
		(func ^. vecFuncSig . funcParams)
	return $ vecFuncBody .~ subBody $ func

subBody :: VecBody -> ReaderT Vars (Fail String) VecBody
subBody = bodyEliminateAssign

bodyEliminateAssign body
	= local (M.union $ body ^. vecBodyVars)
	$ do
		(subResults, subStatements) <- eliminateAssign
			(body ^. vecBodyResults)
			(body ^. vecBodyStatements)
		return
			$ vecBodyStatements .~ subStatements
			$ vecBodyResults .~ subResults
			$ body

eliminateAssign
	:: MonadPlus m
	=> [VecExpression]
	-> [VecStatement]
	-> ReaderT Vars m ([VecExpression], [VecStatement])
eliminateAssign bodyResults [] = return (bodyResults, [])
eliminateAssign bodyResults (statement:statements) = msum
	[ case statement of
		VecAssign name i expr -> do
			let subSingle
				 =  flip runReaderT ((name, i), expr)
				 $  (,)
				<$> mapM substituteSingleAccess bodyResults
				<*> mapM substituteSingleAccess statements
			case subSingle of
				SubstituteSingle subPair -> uncurry eliminateAssign subPair
				SubstituteNone   subPair -> uncurry eliminateAssign subPair
				SubstituteAmbiguous -> mzero
		_ -> mzero
	, do
		(subResults, subStatements) <- eliminateAssign bodyResults statements
		return (subResults, statement:subStatements)
	]


data SubstituteSingle a
	= SubstituteSingle a
	| SubstituteNone a
	| SubstituteAmbiguous

instance Functor SubstituteSingle where
	fmap = liftM

instance Applicative SubstituteSingle where
	pure = return
	(<*>) = ap

instance Monad SubstituteSingle where
	return = SubstituteNone
	SubstituteAmbiguous >>= _ = SubstituteAmbiguous
	SubstituteNone   a >>= f = f a
	SubstituteSingle a >>= f = case f a of
		SubstituteNone b -> SubstituteSingle b
		_ -> SubstituteAmbiguous

class SubstituteSingleAccess a where
	substituteSingleAccess :: a -> ReaderT
		((Name, Integer), VecExpression) SubstituteSingle a

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
	substituteSingleAccess (VecForCycle argIndices argExprs name exprFrom exprTo body) = do
		subExprFrom <- substituteSingleAccess exprFrom
		subExprTo   <- substituteSingleAccess exprTo
		(subArgExprs, subBody) <- ask >>= k . runReaderT
			(mapM substituteSingleAccess argExprs)
		return $ VecForCycle
			argIndices
				subArgExprs
				name
				subExprFrom
				subExprTo
				subBody
		where k = \case
			SubstituteAmbiguous -> lift $ SubstituteAmbiguous
			SubstituteNone subArgExprs
				 -> (subArgExprs,)
				<$> substituteSingleAccess body
			SubstituteSingle subArgExprs
				-- body untouched due to scoping
				 -> return (subArgExprs, body)

instance SubstituteSingleAccess VecBody where
	substituteSingleAccess (VecBody vars statements exprs)
		-- not accounting for scoping
		-- TODO: check if the variable is shadowed
		 =  VecBody vars
		<$> mapM substituteSingleAccess statements
		<*> mapM substituteSingleAccess exprs
