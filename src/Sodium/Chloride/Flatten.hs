module Sodium.Chloride.Flatten (flatten) where

import Control.Applicative
import Control.Lens
import qualified Data.Map as M
import Sodium.Chloride.Program

flatten :: Program -> Program
flatten = overStatements flattenStatements

flattenStatements :: [Statement] -> [Statement]
flattenStatements = concatMap k where
	k (BodyStatement body)
		| M.null (body ^. bodyVars)
		= flattenStatements (body ^. bodyStatements)
	k (ForStatement forCycle)
		= pure $ ForStatement
			$ forBody %~ flattenBody
			$ forCycle
	k (MultiIfStatement multiIfBranch)
		= pure $ MultiIfStatement
			$ tryApply joinMultiIf
			$ multiIfElse %~ flattenBody
			$ multiIfLeafs %~ (map $ _2 %~ flattenBody)
			$ multiIfBranch
	k statement
		= pure statement

flattenBody :: Body -> Body
flattenBody = over bodyStatements flattenStatements

joinMultiIf :: MultiIfBranch -> Maybe MultiIfBranch
joinMultiIf multiIfBranch = case multiIfBranch ^. multiIfElse of
	body | M.null (body ^. bodyVars)
		-> case body ^. bodyStatements of
			[MultiIfStatement multiIfBranch']
				-> Just
				 $ multiIfLeafs %~ (++) (multiIfBranch ^. multiIfLeafs)
				 $ multiIfBranch'
			_ -> Nothing
	_ -> Nothing

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)

overStatements
	= over programFuncs . map
	. over funcBody
	. over bodyStatements
