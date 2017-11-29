// Matheus Amazonas Cabral de Andrade
// s4605640

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
:: Views a = { a :: Sem a,
			   p :: String}

:: Element :== Views Int 
:: Set :== Views [Int]
:: Stmt :== Views ()

show :: String (Views a) -> Views a
show str views = {views & p = str +++ views.p}

// I chose to represent the State as a mapping from Ident
// to Dynamic mainly because I've never used Dynamic and 
// I thought this was the perfect opportunity to dig into
// it. In addition, in a first glance, it looked like a 
// more elegant solution, specially when compared to the
// other alternatives.

unS :: (Sem a) -> State -> Either String (a, State)
unS (S s) = s

// ====== State ======

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

instance Functor Views where
	fmap f v = liftM f v

instance Applicative Views where
	pure x = {a = S (\s -> Right (x, s)), p = ""}
	(<*>) vf va = {a = eval, p = print}
		where
			eval = S $ \s -> case unS vf.a s of
				Right (f,s) = case unS va.a s of
	          		Right (a,s) = Right (f a, s)
	         		Left e = Left e
	      		Left e = Left e
	   		print = vf.p +++ va.p 

instance Monad Views where
	bind x f = {a = eval, p = print}
		where
			eval = S $ \s -> case unS (x.a) s of
				Right (v, s) -> unS ((f v).a) s
				Left e -> Left e
			print = x.p 				// I have no idea how to define this.

// ====== Integer Expressions ======

integer :: Int -> Element 
integer i = {a = pure i,
			 p = toString i}

set :: [Int] -> Set
set s = {a = pure s,
		 p = toString s}

size :: Set -> Element
size s = {a = eval, p = print}
	where
		eval = s.a >>= \set -> pure $ length set
		print = "sizeOf(" +++ s.p +++ ")"

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

instance + (Views a) | + a where
	+ x y = (+) <$> x <*> show "+" y

instance - (Views a) | - a where
	- x y = (-) <$> x <*> show "-" y

instance * (Views a) | * a where
	* x y = (*) <$> x <*> show "*" y

store :: Ident a -> Views () | TC a
store i v = {a = eval, p = ""}
	where
		eval = S $ \s -> Right ((), 'Map'.put i (dynamic v) s)
		print = i +++ " = " +++ v.p

read :: Ident -> Sem Dynamic
read i = S $ \s -> case 'Map'.get i s of
  Just v -> Right (v, s)
  Nothing -> Left ("Variable not found: " +++ i)

fail :: String -> Sem a
fail s = S $ \_ -> Left s

// ====== Set Expressions ======

class Var a where
	variable :: Ident -> a
	(=.) infixl 2 :: Ident a -> Stmt

instance Var Element where
	variable i = { a = eval, p = print}
		where
			eval = read i >>= \var -> case var of
				(x :: Element) -> x.a
				_ -> fail $ "Variable " +++ i +++ " is of type Set, not Int"
			print = "var " +++ i
	(=.) i ux = store i ux

instance Var Set where
	variable i = {a = eval, p = print}
		where
			eval = read i >>= \var -> case var of
				(s :: Set) -> s.a
				_ -> fail $ "Variable " +++ i +++ " is of type Int, not Set"
			print = "var " +++ i
	(=.) i uset = store i uset

// ====== Statements ======

(In) infix 4 :: Element Set -> Views Bool
(In) ue us = {a = eval, p = print}
	where
		eval = ue.a >>= \e -> us.a >>= \set -> pure $ isMember e set
		print = ue.p +++ " in " +++ us.p

Not :: (Views Bool) -> Views Bool
Not e = {a = eval, p = print}
	where
		eval = e.a >>= \v -> pure $ not v
		print = "not " +++ e.p

(||.) infixr 3 :: (Views Bool) (Views Bool) -> Views Bool 
(||.) x y = (||) <$> x <*> show "||" y

(&&.) infixr 3 :: (Views Bool) (Views Bool) -> Views Bool 
(&&.) x y = (&&) <$> x <*> show "&&" y

(==.) infix 4 :: (Views a) (Views a) -> Views Bool | == a 
(==.) x y = (==) <$> x <*> show "==" y

(<=.) infix 4 :: (Views a) (Views a) -> Views Bool | Ord a 
(<=.) x y = (<=) <$> x <*> show "<=" y

If :: (Views Bool) Stmt Stmt -> Stmt
If p t e = {a = eval, p = print}
	where
		eval = p.a >>= \c -> if c t.a e.a
		print = "if (" +++ p.p +++ ") then {\n\t" +++ t.p +++ "}\nelse {\n\t" +++ e.p +++ "}"

(:.) infixl 4 :: Stmt Stmt -> Stmt
(:.) f g = {a = eval, p = print}
	where
		eval = f.a >>| g.a
		print = f.p +++ ";\n" +++ g.p

// I don't know how to define FOR.

/*
For :: Ident Set Stmt -> Stmt
For i uset stmt = {a = eval, p = print}
	where
		eval = uset.a >>= \set -> foldr (:.) (pure ()) (map (exec stmt i) set)
			where
		    	exec :: Stmt Ident Int -> Stmt
		   		exec stmt i v = store i v >>| stmt
		print = "for " +++ i +++ " in " +++ uset.p +++ "{\n\t" +++ stmt.p +++"\n}\n"
*/

run :: (Views a) -> Either String (a, String)
run s = case (unS s.a emptyState) of
		Right (v,_) -> Right (v, s.p)
		Left e -> Left e
	where
		emptyState = 'Map'.newMap

Start = run (If (integer 6 <=. integer 7) ("x" =. integer 42) ("x" =. integer 66))
















