// Matheus Amazonas Cabral de Andrade
// s4605640

module assignment9

import StdEnv
import StdDynamic
import Data.List
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
:: Eval a = Eval (State -> Either String (a, State))
:: Sem a = { eval :: Eval a,
			 print :: [String] -> [String]}

:: Element :== Sem Int 
:: Set :== Sem [Int]
:: Stmt :== Sem ()

show :: String (Sem a) -> Sem a
show str views = { views & print = \c -> [str:views.print c] }

// I chose to represent the State as a mapping from Ident
// to Dynamic mainly because I've never used Dynamic and 
// I thought this was the perfect opportunity to dig into
// it. In addition, in a first glance, it looked like a 
// more elegant solution, specially when compared to the
// other alternatives.

unS :: (Eval a) -> State -> Either String (a, State)
unS (Eval s) = s

// ====== State ======

instance Functor Eval where
	fmap f e = liftM f e

instance Applicative Eval where
	pure x = Eval $ \s -> Right (x, s)
	(<*>) fs ss = fs 
		>>= \f -> ss
		>>= \s -> pure $ f s

instance Monad Eval where
	bind (Eval x) f = Eval $ \s -> case x s of
		Right (v, s) -> unS (f v) s
		Left e -> Left e

// ====== Integer Expressions ======

integer :: Int -> Element 
integer i = { eval = pure i,
			  print = \s -> [toString i:s] }

set :: [Int] -> Set
set s = { eval = pure s,
		  print = \c -> [toString s:c] }

size :: Set -> Element
size s = { eval = fmap length s.eval, print = print }
	where
		print = \c -> ["sizeOf(":s.print[")":c]]

instance + (Sem a) | + a where
	+ e1 e2 = 
		{ eval = (+) <$> e1.eval <*> e2.eval,
		  print = \s -> e1.print["+":e2.print s] }

instance - (Sem a) | - a where
	- e1 e2 = 
		{ eval = (-) <$> e1.eval <*> e2.eval,
		  print = \s -> e1.print["-":e2.print s] }

instance * (Sem a) | * a where
	* e1 e2 = 
		{ eval = (*) <$> e1.eval <*> e2.eval,
		  print = \s -> e1.print["*":e2.print s] }

instance + [a] | == a where
	+ l1 l2 = 'List'.union l1 l2

instance - [a] | == a where
	- l1 l2 = 'List'.difference l1 l2

instance * [a] | == a where
	* l1 l2 = 'List'.intersect l1 l2

(+=) infixl 6 :: Set Element -> Set
(+=) s e = { eval = (\e s -> [e] + s) <$> e.eval <*> s.eval,
			 print = \c -> s.print["+":e.print c] }

(=+) infixl 6 :: Element Set -> Set
(=+) e s = s += e

(-=) infixl 6 :: Set Element -> Set
(-=) s e = { eval = (\e s -> s - [e]) <$> e.eval <*> s.eval,
			 print = \c -> s.print["-":e.print c] }

(=*) infixl 6 :: Element Set -> Set
(=*) e s = { eval = (\e s -> fmap ((*)e) s) <$> e.eval <*> s.eval,
			 print = \c -> e.print["*":s.print c] }

store :: Ident a State -> State | TC a
store i v s = 'Map'.put i (dynamic v) s

read :: Ident -> Eval a | TC a 							
read i = Eval $ \s -> case 'Map'.get i s of
		Just (v :: a^) -> Right (v, s)
		Just d -> Left $ "Expected " +++ toString expType +++ " but got " +++ toString (typeCodeOfDynamic d)
		Nothing -> Left $ "Variable not found: " +++ i
	where
		expType = typeCodeOfDynamic (dynamic undef :: a^)

fail :: String -> Eval a
fail s = Eval $ \_ -> Left s

// ====== Set Expressions ======

variable :: Ident -> Sem a | TC a
variable i = { eval = read i,
			   print = \c -> [i:c] }

(=.) infixl 2 :: Ident (Sem a) -> Sem a | TC a
(=.) i v = { eval = v.eval >>= \val -> Eval (\st -> Right (val, store i v.eval st)),
		     print = \c -> [i,"=":v.print c] }

// ====== Statements ======

(In) infix 4 :: Element Set -> Sem Bool
(In) e s = { eval = (\e s -> isMember e s) <$> e.eval <*> s.eval, 
			 print = \c -> e.print [" in ":s.print c] }

Not :: (Sem Bool) -> Sem Bool
Not e = { eval = fmap not e.eval, 
		  print = \c -> ["not ":e.print c] }

(||.) infixr 3 :: (Sem Bool) (Sem Bool) -> Sem Bool 
(||.) x y = { eval = (||) <$> x.eval <*> y.eval,
			  print = \c -> x.print["||":y.print c] }

(&&.) infixr 3 :: (Sem Bool) (Sem Bool) -> Sem Bool 
(&&.) x y = { eval = (&&) <$> x.eval <*> y.eval,
			  print = \c -> x.print["&&":y.print c] }

(==.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | == a 
(==.) x y = { eval = (==) <$> x.eval <*> y.eval,
			  print = \c -> x.print["==":y.print c] }

(<=.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | Ord a 
(<=.) x y = { eval = (<=) <$> x.eval <*> y.eval,
			  print = \c -> x.print["<=":y.print c] }

If :: (Sem Bool) (Sem a) (Sem a) -> (Sem a)
If p t e = { eval = eval, print = print }
	where
		eval = p.eval >>= \c -> if c t.eval e.eval
		print = \c -> ["if (":p.print[")\nthen{\n":t.print["}\nelse{\n":e.print ["}"]]]]

(:.) infixl 4 :: (Sem a) (Sem b) -> (Sem b)
(:.) f g = { eval = f.eval >>| g.eval, 
			 print = \c -> f.print[";\n":g.print c] }

/*For :: Ident Set (Sem a) -> Sem ()
For i set stmt = { eval = eval, print = print}
	where
		eval = set.eval >>= \s -> foldr (>>|) (pure ()) (map (exec stmt i) s)
			where
		    	exec :: (Sem a) Ident Int -> Eval a
		   		exec stmt i v = i =. 
		print = \c -> ["for",i," in ":set.print [" do{\n":stmt.print ["\n}\n":c]]]*/

run :: (Sem a) -> Either String (a, [String])
run s = case (unS s.eval 'Map'.newMap) of
		Right (v,_) -> Right (v, s.print [])
		Left e -> Left e

Start = run (integer 8)
















