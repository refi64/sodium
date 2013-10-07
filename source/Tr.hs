module Tr
	( Tr(..)
	, state
	, head
	, until
	, before
	, Trap(..)
	, trap
	) where

import Prelude hiding (head, until)
import Control.Monad
import Control.Applicative
import Data.Monoid

newtype Tr x m a = Tr { runTr :: x -> m (a, x) }

instance Monad m => Monad (Tr x m) where
	return a = Tr $
		\x -> return (a, x)
	u >>= f = Tr $
		\x -> do
			(a, y) <- runTr u x
			runTr (f a) y

instance MonadPlus m => MonadPlus (Tr x m) where
	mzero = Tr $
		\x -> mzero
	mplus u v = Tr $
		\x -> mplus (runTr u x) (runTr v x)

instance Monad m => Functor (Tr x m) where
	fmap = liftM

instance Monad m => Applicative (Tr x m) where
	pure  = return
	(<*>) = ap

instance MonadPlus m => Alternative (Tr x m) where
	empty = mzero
	(<|>) = mplus

instance MonadPlus m => Monoid (Tr x m a) where
	mempty  = mzero
	mappend = mplus

state :: Monad m => Tr x m x
state = Tr $
	\x -> return (x, x)

head :: MonadPlus m => Tr [x] m x
head = Tr $
	\x -> case x of
		(y:ys) -> return (y, ys)
		[] -> mzero

until :: MonadPlus m => Tr x m a -> Tr x m b -> Tr x m ([a], Maybe b)
until u v = element `mplus` stop `mplus` none where
	element = (\b -> ([], Just b)) <$> v
	stop = (\a (as, mb) -> (a:as, mb)) <$> u <*> (u `until` v)
	none = return ([], Nothing)

before :: MonadPlus m => Tr x m a -> Tr x m b -> Tr x m ([a], b)
before u v = element `mplus` stop where
	element = (\b -> ([], b)) <$> v
	stop = (\a (as, b) -> (a:as, b)) <$> u <*> (u `before` v)


newtype Trap x m a = Trap { runTrap :: x -> m a }

trap :: Monad m => Tr x m (a -> b) -> Trap x m a -> Trap x m b
trap tr next = Trap $
	\x -> do
		(a, y) <- runTr tr x
		b <- runTrap next y
		return $ a b

instance MonadPlus m => Monoid (Trap x m a) where
	mempty = Trap $
		\x -> mzero
	mappend u v = Trap $
		\x -> mplus (runTrap u x) (runTrap v x)
