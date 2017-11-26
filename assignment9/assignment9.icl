module assignment9

import StdEnv
import StdDynamic
import Data.Either
import Data.Functor
import Data.Maybe
import Control.Applicative 
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
from Data.Func import $

:: Ident :== String
:: State :== 'Map'.Map Ident Dynamic
:: Sem a = S (State -> Either String (a, State))

:: Element :== Sem Int 
:: Set :== Sem [Int]

// I chose to represent the State as a mapping from Ident
// to Dynamic mainly because I've never used Dynamic and 
// I thought this was the perfect opportunity to dig into
// it. In addition, in a first glance, it looked like a 
// more elegant solution, specially when compared to the
// other alternatives.

unS :: (Sem a) -> State -> Either String (a, State)
unS (S s) = s

instance Functor Sem where
	fmap f e = liftM f e

instance Applicative Sem where
	pure x = S $ \s -> Right (x, s)
	(<*>) fs ss = fs 
		>>= \f -> ss
		>>= \s -> pure $ f s

instance Monad Sem where
	bind (S x) f = S $ \s -> case x s of
		Right (v, s) -> unS (f v) s
		Left e -> Left e

integer :: Int -> Element 
integer i = pure i

size :: Set -> Element
size s = s
	>>= \set -> pure $ length set

instance + Element where
	(+) e1 e2 = e1 >>= \v1 -> e2 >>= \v2 -> pure $ v1 + v2

instance - Element where
	(-) e1 e2 = e1 >>= \v1 -> e2 >>= \v2 -> pure $ v1 - v2

instance * Element where
	(*) e1 e2 = e1 >>= \v1 -> e2 >>= \v2 -> pure $ v1 * v2

instance + Set where
	(+) s1 s2 = s1 >>= \v1 -> s2 >>= \v2 -> pure $ 'List'.union v1 v2

instance - Set where
	(-) s1 s2 = s1 >>= \v1 -> s2 >>= \v2 -> pure $ 'List'.difference v1 v2

instance * Set where
	(*) s1 s2 = s1 >>= \v1 -> s2 >>= \v2 -> pure $ 'List'.intersect v1 v2

union :: Set Element -> Set
union us ue = us >>= \s -> ue >>= \e -> pure $ 'List'.union s [e]

union` :: Element Set -> Set
union` ue us = us >>= \s -> ue >>= \e -> pure $ 'List'.union s [e]

difference :: Set Element -> Set
difference us ue = us >>= \s -> ue >>= \e -> pure $ 'List'.difference s [e]

intersect :: Element Set -> Set
intersect ux us = us >>= \s -> ux >>= \x -> pure $ map ((*)x) s

eval :: (Sem a) -> State -> Either String (a, State)
eval (S e) = e

store :: Ident a -> Sem a | TC a
store i v = S $ \s -> Right (v, 'Map'.put i (dynamic v) s)

read :: Ident -> Sem Dynamic
read i = S $ \s -> case 'Map'.get i s of
  Just v -> Right (v, s)
  Nothing -> Left ("Variable not found: " +++ i)

fail :: String -> Sem a
fail s = S $ \_ -> Left s

class Var a where
	variable :: Ident -> a
	(=.) infixl 2 :: Ident a -> a

instance Var Element where
	variable i = read i >>= \var -> case var of
		(x :: Element) -> x
		_ -> fail $ "Variable " +++ i +++ " is of type Set, not Int"
	(=.) i ux = ux >>= \v -> store i v

instance Var Set where
	variable i = read i >>= \var -> case var of
		(s :: Set) -> s
		_ -> fail $ "Variable " +++ i +++ " is of type Int, not Set"
	(=.) i uset = uset >>= \set -> store i set


