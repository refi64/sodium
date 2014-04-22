module Sodium.Chloride.Pattern (sub) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens hiding (Index, Fold)
import qualified Data.Map as M
import Sodium.Chloride.Program.Vector
import Sodium.ApplyOnce

sub :: Program -> Program
sub = programFuncs . traversed %~ subFunc

subFunc :: Func -> Func
subFunc func = maybe func id $ do
	subBody <- runReaderT
		(subBody $ func ^. funcBody)
		(func ^. funcSig . funcParams)
	return $ funcBody .~ subBody $ func

subBody :: (Alternative m, MonadPlus m) => Body -> ReaderT Vars m Body
subBody = bodyEliminateAssign

bodyEliminateAssign body
	= local (M.union $ body ^. bodyVars)
	$ update body <$> eliminateAssign
		(body ^. bodyResults)
		(body ^. bodyStatements)
	where update body (subResults, subStatements)
		= bodyResults .~ subResults
		$ bodyStatements .~ subStatements
		$ body

eliminateAssign
	:: (Alternative m, MonadPlus m)
	=> [Expression]
	-> [(IndicesList, Statement)]
	-> ReaderT Vars m ([Expression], [(IndicesList, Statement)])
eliminateAssign bodyResults [] = return (bodyResults, [])
eliminateAssign bodyResults ((indices, statement):statements)
	= (<|> follow statement)
	$ case statement of
		Assign expr -> do
			name <- case indices of
				[name] -> return name
				_ -> mzero
			let subSingle = liftA2 (,)
				(mapM substituteSingleAccess bodyResults)
				(mapM (_2 substituteSingleAccess) statements)
			case runReaderT subSingle (name, expr) of
				Once subPair -> uncurry eliminateAssign subPair
				None subPair -> uncurry eliminateAssign subPair
				Ambiguous -> mzero
		statement -> (touch >=> follow) statement
	where
		follow statement
			 =  over _2 ((indices, statement):)
			<$> eliminateAssign bodyResults statements
		touch (ForStatement forCycle)
			= ForStatement <$> forBody bodyEliminateAssign forCycle
		touch (MultiIfStatement multiIfBranch)
			= MultiIfStatement <$> k bodyEliminateAssign multiIfBranch
			where k = (>=>) <$> multiIfLeafs . traversed . _2 <*> multiIfElse
		touch (BodyStatement body)
			= BodyStatement <$> bodyEliminateAssign body
		touch _ = mzero


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
		_ -> lift $ Ambiguous

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
