module Sodium.Chloride.Flatten (flatten) where

import Control.Applicative
import Control.Monad
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program

flatten :: Program -> Program
flatten = programFuncs . traversed . funcBody  %~ flattenBody

flattenStatements :: [Statement] -> [Statement]
flattenStatements = concatMap flattenStatement where
	flattenStatement (BodyStatement body)
		| M.null (body ^. bodyVars)
		= flattenStatements (body ^. bodyStatements)
	flattenStatement statement
		= pure (onFor . onMultiIf $ statement)
	onMultiIf = _MultiIfStatement
		%~ tryApply joinMultiIf . (k %~ flattenBody)
		where k = liftA2 (>=>) (multiIfLeafs . traversed . _2) multiIfElse
	onFor = _ForStatement %~ (forBody %~ flattenBody)

flattenBody :: Body -> Body
flattenBody = bodyStatements %~ flattenStatements

joinMultiIf :: MultiIfBranch -> Maybe MultiIfBranch
joinMultiIf multiIfBranch
	 = multiIfBranch ^? multiIfElse . bodySingleton . _MultiIfStatement
	<&> \multiIfBranch' -> tryApply joinMultiIf multiIfBranch'
		& multiIfLeafs %~ (++) (multiIfBranch ^. multiIfLeafs)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
