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
			$ multiIfElse %~ flattenBody
			$ multiIfLeafs %~ (map $ _2 %~ flattenBody)
			$ multiIfBranch
	k statement
		= pure statement

flattenBody :: Body -> Body
flattenBody = over bodyStatements flattenStatements

overStatements
	= over programFuncs . map
	. over funcBody
	. over bodyStatements
