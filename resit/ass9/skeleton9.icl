module skeleton9

import StdMisc
import StdDynamic
import StdString
import StdBool
import Data.Either
import Data.Functor
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
from Data.Func import $ 


:: Set     :== Sem [Int]
:: Elem    :== Sem Int
:: Ident   :== String
:: State   :== 'Map'.Map Ident Dynamic
:: Eval a = S (State -> Either String (a, State))
:: Sem a = {eval :: Eval a, print :: [String] -> [String]}

unE :: (Eval a) -> State -> Either String (a, State)
unE (S s) = s

fail :: String -> Eval a
fail s = S \_ -> Left s

instance Functor Eval where
	fmap f (S m) = S \s -> case m s of
		Right (a,s) = Right (f a, s)
		Left e = Left e

instance Applicative Eval where
	pure x = S \s -> Right (x,s)
	(<*>) (S mf) (S mx) = S \s -> case mf s of 
		Right (f,s) = case mx s of
			Right (x,s) = Right (f x, s)
			Left e = Left e
		Left e = Left e

instance Monad Eval where
	bind (S mx) f = S \s -> case mx s of
		Right (x,s) = unE (f x) s
		Left e = Left e

instance + (Sem a) | + a where
	(+) e1 e2 = { eval = (+) <$> e1.eval <*> e2.eval,
	              print = \p -> e1.print ["+":e1.print p]}

instance - (Sem a) | - a where
	(-) e1 e2 = { eval = (-) <$> e1.eval <*> e2.eval,
	              print = \p -> e1.print ["-":e1.print p]}

instance * (Sem a) | * a where
	(*) e1 e2 = { eval = (*) <$> e1.eval <*> e2.eval,
	              print = \p -> e1.print ["*":e1.print p]}

instance + [a] | == a where
	(+) l1 l2 = 'List'.union l1 l2

instance - [a] | == a where
	(-) l1 l2 = 'List'.difference l1 l2

instance * [a] | == a where
	(*) l1 l2 = 'List'.intersect l1 l2

(+=) infixl 6 :: Elem Set -> Set
(+=) e s = { eval = (\e s -> [e] + s) <$> e.eval <*> s.eval,
             print = \p -> e.print ["+":s.print p]}

(=+) infixl 6 :: Set Elem  -> Set
(=+) s e = { eval = (\s e -> s + [e]) <$> s.eval <*> e.eval,
             print = \p -> s.print ["+":e.print p]}

(=-) infixl 6 :: Set Elem  -> Set
(=-) s e = { eval = (\s e -> s - [e]) <$> s.eval <*> e.eval,
             print = \p -> s.print ["-":e.print p]}

(=*) infixl 7 :: Set Elem  -> Set
(=*) s e = { eval = (\s e -> fmap ((*)e) s) <$> s.eval <*> e.eval,
             print = \p -> s.print ["*":e.print p]}

eval :: (Sem a) -> Either String (a, State)
eval s = unE s.eval 'Map'.newMap

store :: Ident (Eval a) -> Eval a | TC a
store i (S e) = S \s -> case e s of
	Right (a,s) = Right (a, 'Map'.put i (dynamic a) s)
	Left e = Left e

read :: Ident -> Eval a | TC a
read i = S \s -> case 'Map'.get i s of
	Just (x :: a^) = Right (x, s)
	Just _ = Left $ "Varibale " +++ i +++ " type doesn't match."
	_ = Left $ "Variable " +++ i +++ " not found."

integer :: Int -> Elem
integer x = {eval = S \s -> Right (x,s), print = \p -> [toString x:p]}

set :: [Int] -> Set
set set = { eval = pure set,
            print = \p -> [toString set:p]}

size :: Set -> Elem
size set = { eval = fmap length set.eval,
             print = \p -> ["sizeOf(":set.print[")":p]]}

var :: Ident -> Sem a | TC a
var i = { eval = read i, print = \p -> [i:p]}

(=.) infixl 2 :: Ident (Sem a) -> Sem a | TC a
(=.) id s = { eval = store id s.eval,
              print = \p -> [id,"=":s.print p]}

(In) infix 4 :: Elem Set -> Sem Bool
(In) e s = { eval = isMember <$> e.eval <*> s.eval,
             print = \p -> e.print [" in ":s.print p]}

(==.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | == a
(==.) s1 s2 = { eval = (==) <$> s1.eval <*> s2.eval,
                print = \p -> s1.print ["==":s2.print p]}

(<.) infix 4 :: Elem Elem -> Sem Bool 
(<.) s1 s2 = { eval = (<) <$> s1.eval <*> s2.eval,
                print = \p -> s1.print ["<":s2.print p]}

(||.) infix 4 :: (Sem Bool) (Sem Bool) -> Sem Bool 
(||.) s1 s2 = { eval = (||) <$> s1.eval <*> s2.eval,
                print = \p -> s1.print ["||":s2.print p]}

(&&.) infix 4 :: (Sem Bool) (Sem Bool) -> Sem Bool
(&&.) s1 s2 = { eval = (&&) <$> s1.eval <*> s2.eval,
                print = \p -> s1.print ["&&":s2.print p]}

(:.) infixl 4 :: (Sem a) (Sem b) -> Sem b
(:.) sa sb = { eval = sa.eval >>| sb.eval,
               print = \p -> sa.print [";":sb.print p]}

If :: (Sem Bool) (Sem a) (Sem a) -> Sem a
If sc st se = { eval = sc.eval >>= \c -> if c st.eval se.eval,
                print = \p -> ["if ":sc.print [" then ":st.print [" else ":se.print p]]]}             

For :: Ident Set (Sem a) -> Sem ()
For i ss stmt = { eval = eval,
                 print = \p -> ["for ",i," in ":ss.print [" do ":stmt.print p]]}
where
	eval :: Eval ()
	eval = ss.eval >>= \s -> case s of
		[] = pure ()
		[x:xs] = store i (integer x).eval >>| stmt.eval >>| (For i (set xs) stmt).eval

Start = eval (integer 8 += set [1,2,3,4,5,6,7,9])