definition module monad

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming, week 4
*/

import StdMisc

class MFunctor f where
	fmap :: (a->b) (f a) -> (f b)
	(<$>) infixl 4 :: (a->b) (f a) -> (f b) | MFunctor f
 	(<$>) f x :== fmap f x

class MApplicative f | MFunctor f where
	pure :: a -> f a
	(<*>) infixl 4 :: (f (a->b)) (f a) -> f b

class MMonad m | MApplicative m where
	bind :: (m a) (a->m b) -> m b
	(>>=) infixl 1 :: (m a) (a->m b) -> m b | MMonad m
	(>>=) a f :== bind a f
	(>>|) infixl 1 :: (m a)    (m b) -> m b | MMonad m
	(>>|) a b :== a >>= \_.b
	rtrn :: a -> m a | MMonad m
	rtrn a :== pure a

class fail m | MApplicative m where
	fail :: m a
	guard :: Bool -> m a | fail m
	guard b :== if b (pure undef) fail

class OrMonad m where
	(<|>) infixl 0 :: (m a) (m a) -> m a

