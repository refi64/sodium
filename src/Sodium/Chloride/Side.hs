module Sodium.Chloride.Side (side) where

import Control.Applicative
import Control.Monad.Writer
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program
import Data.Stack

side :: Program -> Program
side = programFuncs . traversed . funcBody %~ sideBody

sideBody :: Body -> Body
sideBody = bodyStatements . traversed %~ sideStatement

sideStatement :: Statement -> Statement
sideStatement = onAssign . onFor . onMultiIf . onBody where
	onBody = _BodyStatement %~ sideBody
	onMultiIf = _MultiIfStatement %~ (k %~ sideBody) where
		k = liftA2 (>=>) (multiIfLeafs . traversed . _2) multiIfElse
	onFor = _ForStatement %~ (forBody %~ sideBody)
	onAssign = \case
		Assign name expr -> BodyStatement (sideAssign name expr)
		statement -> statement

sideAssign :: Name -> Expression -> Body
sideAssign name expr = Body (M.fromList vardecls) statements where
	statements = sidecalls ++ [SideCall name OpId [e]]
	(e, xs) = evalStack (runWriterT (sideExpression expr)) (map NameGen [0..])
	(vardecls, sidecalls) = unzip xs

sideExpression
	:: Expression
	-> WriterT [((Name, ClType), Statement)] (Stack Name) Expression
sideExpression = \case
	Access name -> return (Access name)
	Primary lit -> return (Primary lit)
	Call op args -> do
		eArgs <- mapM sideExpression args
		name <- pop
		let vardecl = (name, error "NO TYPE")
		tell [(vardecl, SideCall name op eArgs)]
		return (Access name)
