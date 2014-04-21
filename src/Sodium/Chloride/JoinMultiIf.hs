module Sodium.Chloride.JoinMultiIf (joinMultiIf) where

import Control.Lens
import Sodium.Chloride.Program.Scalar
import Sodium.Chloride.Recmap.Scalar

joinMultiIf :: Program -> Program
joinMultiIf = recmap $ recmapper
	{ recmapStatement = _MultiIfStatement %~ tryApply joinMultiIf' }

joinMultiIf' :: MultiIfBranch -> Maybe MultiIfBranch
joinMultiIf' multiIfBranch
	 =  multiIfBranch ^? multiIfElse . bodySingleton . _MultiIfStatement
	<&> over multiIfLeafs (view multiIfLeafs multiIfBranch ++)

tryApply :: (a -> Maybe a) -> (a -> a)
tryApply f a = maybe a id (f a)
