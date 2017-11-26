module assignment9

import StdDynamic
import Data.Either
import Data.Functor
import Control.Applicative 
import Control.Monad
import qualified Data.Map as Map
from Data.Func import $


Start = 1

:: Ident :== String
:: State :== 'Map'.Map Ident Dynamic
:: Sem a = S (State -> Either String (a, State))

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
