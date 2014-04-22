{-# LANGUAGE RankNTypes #-}
module Sodium.Chloride.Recmap.Scalar
	( Recmapper
	, Recmap
	, recmapper
	, defaultRecmapper
	, recmap
	, recmapProgram
	, recmapFunc
	, recmapBody
	, recmapStatement
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Sodium.Chloride.Program.Scalar

recmapProgram :: Recmapper -> Program -> Program
recmapProgram rm = programFuncs . traversed %~ recmapFunc rm

recmapFunc :: Recmapper -> Func -> Func
recmapFunc rm = funcBody %~ recmap rm

data Recmapper = Recmapper
	{ recmapStatement :: Statement -> Statement
	, recmapBody :: Body -> Body
	}

defaultRecmapper = Recmapper id id

class Recmap a where
	recmapper :: (a -> a) -> Recmapper
	recmapmod :: Recmapper -> (a -> a)
	recmapdiv :: (forall b. Recmap b => b -> b) -> (a -> a)

recmap :: Recmapper -> (forall a. Recmap a => a -> a)
recmap rm = recmapmod rm . recmapdiv (recmap rm)

instance Recmap Body where
	recmapper rm = defaultRecmapper { recmapBody = rm }
	recmapmod rm = recmapBody rm
	recmapdiv rm = bodyStatements . traversed %~ rm

instance Recmap Statement where
	recmapper rm = defaultRecmapper { recmapStatement = rm }
	recmapmod rm = recmapStatement rm
	recmapdiv rm = onMultiIf . onBody . onFor where
		onMultiIf = _MultiIfStatement %~ (k %~ rm)
			where k = liftA2 (>=>) (multiIfLeafs . traversed . _2) multiIfElse
		onFor = _ForStatement %~ (forBody %~ rm)
		onBody = _BodyStatement %~ rm
