module Sodium.Chloride.IOMagic (uncurse) where

import Control.Applicative
import Control.Monad.Reader
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program

data Error
	= NoAccess Name
	deriving (Show)

type M a = a -> ReaderT Vars (Either Error) a

uncurse :: Program -> Program
uncurse program
	= either (error . show) id
	$ (programFuncs . traversed) uncurseFunc program

uncurseFunc :: Func -> Either Error Func
uncurseFunc func
	= runReaderT
		(uncurseBody $ func ^. funcBody)
		(func ^. funcSig . funcParams)
	<&> flip (set funcBody) func

uncurseBody :: M Body
uncurseBody body
	= local (M.union $ body ^. bodyVars)
	$ (bodyStatements . traversed) uncurseStatement body

uncurseStatement :: M Statement
uncurseStatement (Execute name args) = case name of
	ExecuteRead _ -> case args of
		[LValue name]
			 -> lookupType name
			<&> \t ->Execute (ExecuteRead t) [LValue name]
		_ -> error "IOMagic supports only single-value read operations"
	ExecuteWrite
		 -> Execute ExecuteWrite
		<$> mapM uncurseArgument args
	_ -> return (Execute name args)
uncurseStatement (ForStatement forCycle)
	= ForStatement <$> forBody uncurseBody forCycle
uncurseStatement (MultiIfStatement multiIfBranch)
	= MultiIfStatement <$> k uncurseBody multiIfBranch
	where k = (>=>) <$> multiIfLeafs . traversed . _2 <*> multiIfElse
uncurseStatement (BodyStatement body)
	= BodyStatement <$> uncurseBody body
uncurseStatement statement
	= return statement

uncurseArgument :: M Argument
uncurseArgument = \case
	-- TODO: apply `show` only to non-String
	-- expressions as soon as typecheck is implemented
	RValue expr -> return $ RValue expr
	LValue name
		 -> lookupType name
		<&> \case
			ClString -> RValue $ Access name
			_ -> RValue $ Call (CallOperator OpShow) [Access name]

lookupType name = do
	vars <- ask
	lift $ maybe (Left $ NoAccess name) Right (M.lookup name vars)
