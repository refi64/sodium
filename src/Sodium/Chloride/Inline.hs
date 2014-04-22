module Sodium.Chloride.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens hiding (Index, Fold)
import qualified Data.Map as M
import Sodium.Chloride.Program.Vector
import Sodium.Chloride.Recmap.Vector
import Sodium.ApplyOnce

inline :: Program -> Program
inline = recmapProgram' (recmapper' inlineBody)

inlineBody body
	= update body $ eliminateAssign
		(body ^. bodyResults, body ^. bodyStatements)
	where update body (subResults, subStatements)
		= bodyResults .~ subResults
		$ bodyStatements .~ subStatements
		$ body

eliminateAssign
	:: ([Expression], [(IndicesList, Statement)])
	-> ([Expression], [(IndicesList, Statement)])
eliminateAssign (bodyResults, (statement:statements))
	= maybe follow id $ do
		([name], Assign expr) <- Just statement
		let subSingle = liftA2 (,)
			(mapM substituteSingleAccess bodyResults)
			(mapM (_2 substituteSingleAccess) statements)
		case runReaderT subSingle (name, expr) of
			Once bodyPair -> Just (eliminateAssign bodyPair)
			None bodyPair -> Just (eliminateAssign bodyPair)
			Ambiguous -> Nothing
	where follow
		= over _2 (statement:)
		$ eliminateAssign (bodyResults, statements)
eliminateAssign bodyPair = bodyPair

type SubstituteAccessEnv = ((Name, Index), Expression)

class SubstituteSingleAccess a where
	substituteSingleAccess :: a -> ReaderT SubstituteAccessEnv ApplyOnce a

instance SubstituteSingleAccess Expression where
	substituteSingleAccess = \case
		Primary prim -> return $ Primary prim
		Access name' j -> do
			(name, expr) <- ask
			if name == (name', j)
				then lift (Once expr)
				else return (Access name' j)
		Call op exprs -> do
			Call op <$> mapM substituteSingleAccess exprs
		Fold op exprs range -> do
			Fold op
				<$> mapM substituteSingleAccess exprs
				<*> substituteSingleAccess range

instance SubstituteSingleAccess Statement where
	substituteSingleAccess = \case
		-- It is assumed that every variable is assigned only once,
		-- since the code is vectorized. Therefore it's not the
		-- variable we are substituting, and no additional checks required.
		Assign expr -> Assign <$> substituteSingleAccess expr
		Execute executeName args
			 -> Execute executeName
			<$> mapM substituteSingleAccess args
		ForStatement forCycle
			 -> ForStatement
			<$> substituteSingleAccess forCycle
		MultiIfStatement multiIfBranch
			 -> MultiIfStatement
			<$> substituteSingleAccess multiIfBranch
		BodyStatement body
			 -> BodyStatement
			<$> substituteSingleAccess body

instance SubstituteSingleAccess ForCycle where
	substituteSingleAccess
		 =  forRange substituteSingleAccess
		>=> (forArgExprs . traversed) substituteSingleAccess
		>=> liftA2 (>>=)
			(shadowedBy . toListOf (forArgIndices . traversed . _1))
			(flip subBody)
		where subBody shadowed
			| shadowed  = return
			| otherwise = forBody substituteSingleAccess

instance SubstituteSingleAccess MultiIfBranch where
	substituteSingleAccess
		 =  (multiIfLeafs . traversed)
		 	(_1 substituteSingleAccess >=> _2 substituteSingleAccess)
		>=> multiIfElse substituteSingleAccess

instance SubstituteSingleAccess Body where
	substituteSingleAccess
		= liftA2 (>>=)
			(shadowedBy . M.keys . view bodyVars)
			(flip subBody)
		where subBody shadowed
			| shadowed = return
			| otherwise
				 =  (bodyStatements . traversed . _2) substituteSingleAccess
				>=> (bodyResults . traversed) substituteSingleAccess

shadowedBy :: Monad m => [Name] -> ReaderT SubstituteAccessEnv m Bool
shadowedBy names = do
	(name, _) <- ask
	return $ fst name `elem` names
