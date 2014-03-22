module Sodium.SubstituteSingle where

import Control.Applicative
import Control.Monad

data SubstituteSingle a
	= SubstituteSingle a
	| SubstituteNone a
	| SubstituteAmbiguous

instance Functor SubstituteSingle where
	fmap = liftM

instance Applicative SubstituteSingle where
	pure = return
	(<*>) = ap

instance Monad SubstituteSingle where
	return = SubstituteNone
	SubstituteAmbiguous >>= _ = SubstituteAmbiguous
	SubstituteNone   a >>= f = f a
	SubstituteSingle a >>= f = case f a of
		SubstituteNone b -> SubstituteSingle b
		_ -> SubstituteAmbiguous
