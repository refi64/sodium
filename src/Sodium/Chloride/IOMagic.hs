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
uncurseFunc func = runReaderT
	(funcBody uncurseBody func)
	(func ^. funcSig . funcParams)

uncurseBody :: M Body
uncurseBody body
	= local (M.union $ body ^. bodyVars)
	$ (bodyStatements . traversed) uncurseStatement body

uncurseStatement :: M Statement
uncurseStatement = onExecute >=> onFor >=> onMultiIf >=> onBody where
	onFor = _ForStatement (forBody uncurseBody)
	onMultiIf = _MultiIfStatement (k uncurseBody)
		where k = (>=>) <$> multiIfLeafs . traversed . _2 <*> multiIfElse
	onBody = _BodyStatement uncurseBody
	onExecute = _Execute $ \(mres, name, args) -> case name of
		OpReadLn _ -> case (mres, args) of
			(Nothing, [LValue name])
				 -> lookupType name
				<&> \t -> (Just name, OpReadLn t, [])
			_ -> error "IOMagic supports only single-value read operations"
		OpPrintLn
			 -> mapM uncurseArgument args
			<&> \args -> (mres, OpPrintLn, args)
		_ -> return (mres, name, args)

uncurseArgument :: M Argument
uncurseArgument = \case
	-- TODO: apply `show` only to non-String
	-- expressions as soon as typecheck is implemented
	RValue expr -> return $ RValue expr
	LValue name
		 -> lookupType name
		<&> \case
			ClString -> RValue $ Access name
			_ -> RValue $ Call OpShow [Access name]

lookupType name = do
	vars <- ask
	lift $ maybe (Left $ NoAccess name) Right (M.lookup name vars)
