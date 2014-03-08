module Sodium.Success where

import Control.Applicative
import Control.Monad

data Fail x a
	= Success a
	| Fail Integer [x]
	deriving (Show)

instance Monad (Fail x) where
	return = Success
	Success a >>= f = f a
	Fail i xs >>= f = Fail i xs

instance MonadPlus (Fail x) where
	mzero = Fail 0 []
	mplus (Success a) _ = Success a
	mplus _ (Success a) = Success a
	mplus (Fail i xs) (Fail j ys)
		| i > j = Fail i xs
		| i < j = Fail j ys
		| otherwise = Fail i (xs ++ ys)

instance Functor (Fail x) where
	fmap = liftM

instance Applicative (Fail x) where
	pure = return
	(<*>) = ap

instance Alternative (Fail x) where
	empty = mzero
	(<|>) = mplus

annotate :: Maybe a -> Integer -> x -> Fail x a
annotate (Just a) _ _ = Success a
annotate Nothing i x = Fail i [x]

flatten :: Fail x a -> Either [x] a
flatten (Success a) = Right a
flatten (Fail _ xs) = Left xs
