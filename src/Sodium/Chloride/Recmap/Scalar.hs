{-# LANGUAGE RankNTypes #-}
module Sodium.Chloride.Recmap.Scalar
	( Recmapper (..)
	, Recmap(..)
	, recmapper
	, recmap
	) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Sodium.Chloride.Program.Scalar

data Recmapper = Recmapper
	{ recmapStatement :: Statement -> Statement
	, recmapBody :: Body -> Body
	}

recmapper = Recmapper id id

class Recmap a where
	recmapmod :: Recmapper -> (a -> a)
	recmapmod _ = id
	recmapdiv :: (forall b. Recmap b => b -> b) -> (a -> a)

recmap :: Recmapper -> (forall a. Recmap a => a -> a)
recmap rm = recmapmod rm . recmapdiv (recmap rm)

instance Recmap Program where
	recmapdiv rm = programFuncs . traversed . funcBody %~ rm

instance Recmap Body where
	recmapmod = recmapBody
	recmapdiv rm = bodyStatements . traversed %~ rm

instance Recmap Statement where
	recmapmod = recmapStatement
	recmapdiv rm = onMultiIf . onBody . onFor where
		onMultiIf = _MultiIfStatement %~ (k %~ rm)
			where k = liftA2 (>=>) (multiIfLeafs . traversed . _2) multiIfElse
		onFor = _ForStatement %~ (forBody %~ rm)
		onBody = _BodyStatement %~ rm
