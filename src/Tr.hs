module Tr
	( Tr
	, runTr
	, state
	, head
	, until
	, before
	, fallback
	, trap
	, trapGuard
	, expect
	) where

import Prelude hiding (head, until)
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy

type Tr = StateT
runTr = runStateT

head :: MonadPlus m => Tr [x] m x
head = StateT $
	\x -> case x of
		(y:ys) -> return (y, ys)
		[] -> mzero

until :: (Functor m, MonadPlus m) => Tr x m a -> Tr x m b -> Tr x m ([a], Maybe b)
until u v = element `mplus` stop `mplus` none where
	element = (\b -> ([], Just b)) <$> v
	stop = (\a (as, mb) -> (a:as, mb)) <$> u <*> (u `until` v)
	none = return ([], Nothing)

before :: (Functor m, MonadPlus m) => Tr x m a -> Tr x m b -> Tr x m ([a], b)
before u v = element `mplus` stop where
	element = (\b -> ([], b)) <$> v
	stop = (\a (as, b) -> (a:as, b)) <$> u <*> (u `before` v)

fallback :: Alternative f => a -> f a -> f a
fallback = flip (<|>) . pure

trap :: Monad m => Tr x m (a -> b) -> (x -> m a) -> (x -> m b)
trap tr next =
	\x -> do
		(a, y) <- runTr tr x
		b <- next y
		return $ a b

trapGuard :: (Functor m, MonadPlus m) => Tr x m b -> (x -> Bool) -> (x -> m b)
trapGuard tr p = Tr.trap (const <$> tr) (guard . p)

expect :: (Eq x, MonadPlus m) => x -> Tr [x] m x
expect x = mfilter (==x) head
