module skeleton10

import StdMisc
import StdDynamic
import StdBool
import StdString
import Data.List
import Data.Either
import Data.Maybe
import Data.Functor
import Control.Applicative
import Control.Monad
from StdFunc import id, o
from Data.Func import $
import qualified Data.Map as Map

:: Expression a = 
	  New      (BM a [Int]) [Int]
	| Elem     (BM a Int) Int
	| Size     (BM a Int) Set
	| (+.) infixl 6 (Expression a) (Expression a)
	| (-.) infixl 6 (Expression a) (Expression a)
	| (*.) infixl 7 (Expression a) (Expression a)
	| Add (BM a [Int]) Elem Set
	| Delete (BM a [Int]) Set Elem
	| Variable Ident 
	| Att Ident (Expression a)

:: Logical = 
	  TRUE | FALSE
	| (In) infix 4 Elem Set
	| E.a: (==.) infix 4 (Expression a) (Expression a) & +, -, *, TC, ==, <  a
	| (<.) infix 4 Elem Elem
	| Not Logical
	| (||.) infixr 2 Logical Logical
	| (&&.) infixr 3 Logical Logical

:: Stmt = 
	  If Logical Stmt Stmt
	| For Ident Set Stmt
	| E.a: Expression (Expression a) & +, -, *, TC, ==, <  a
	| Logical Logical
	| (:.) infixl 1 Stmt Stmt

:: Set    :== Expression [Int]
:: Elem  :== Expression Int
:: Ident  :== String

:: State :== 'Map'.Map Ident Dynamic
:: Sem a = S (State -> (Either String (a, State)))

unS :: (Sem a) -> State -> (Either String (a, State))
unS (S s) = s

store :: Ident a -> Sem a | TC a
store i a = S \s -> Right(a, 'Map'.put i (dynamic a) s)

read :: Ident -> Sem a | TC a
read i = S \s -> case 'Map'.get i s of
	Just (x :: a^) = Right (x,s)
	Just _ = Left $ "Varibale " +++ i +++ " type doesn't match."
	_ = Left $ "Variable " +++ i +++ " not found."

:: BM a b = { t :: a -> b, f :: b -> a}

bm :: BM a a
bm = { t = id, f = id }

instance Functor Sem where
	fmap f (S m) = S \s -> case m s of
		Right (a,s) = Right (f a, s)
		Left e = Left e

instance Applicative Sem where
	pure x = S \s -> Right (x,s)
	(<*>) (S mf) (S ma) = S \s -> case mf s of
		Right (f,s) = case ma s of
			Right(a,s) = Right (f a,s)
			Left e = Left e
		Left e = Left e

instance Monad Sem where
	bind (S ma) f = S \s -> case ma s of
		Right(a,s) = unS (f a) s
		Left e = Left e

instance + [a] | == a where
	(+) l1 l2 = union l1 l2

instance - [a] | == a where
	(-) l1 l2 = difference l1 l2

instance * [a] | == a where
	(*) l1 l2 = intersect l1 l2

set :: [Int] -> Set
set l = New bm l

int :: Int -> Elem
int i = Elem bm i

evalE :: (Expression a) -> Sem a | +, -, *, TC, ==, < a
evalE (New bm set) = pure $ bm.f set
evalE (Elem bm i) = pure $ bm.f i
evalE (Size bm set) = evalE set >>= \s -> pure $ bm.f $ length s
evalE (e1 +. e2) = (+) <$> evalE e1 <*> evalE e2
evalE (e1 -. e2) = (-) <$> evalE e1 <*> evalE e2
evalE (e1 *. e2) = (*) <$> evalE e1 <*> evalE e2
evalE (Add bm e s) = (\e s -> bm.f (union s [e])) <$> evalE e <*> evalE s
evalE (Delete bm s e) = (\e s -> bm.f (difference s [e])) <$> evalE e <*> evalE s
evalE (Variable i) = read i
// evalE (VarAttEle bm i e) = store i $ bm.f <$> evalE e
// evalE (VarAttSet bm i s) = store i $ bm.f <$> evalE s
evalE (Att i e) = evalE e >>= store i

evalL :: Logical -> Sem Bool
evalL TRUE = pure $ True
evalL FALSE = pure $ False
evalL (e In s) = isMember <$> evalE e <*> evalE s
evalL (e1 ==. e2) = (==) <$> evalE e1 <*> evalE e2
evalL (e1 <. e2) = (<) <$> evalE e1 <*> evalE e2
evalL (Not l) = not <$> evalL l
evalL (l1 ||. l2) = (||) <$> evalL l1 <*> evalL l2
evalL (l1 &&. l2) = (&&) <$> evalL l1 <*> evalL l2

evalS :: Stmt -> Sem ()
evalS (If c t e) = evalL c >>= \c -> if c (evalS t) (evalS e)
evalS (For i ms stmt) = evalE ms >>= \s -> case s of
	[] = pure ()
	[x:xs] = store i x >>| evalS stmt >>| evalS (For i (set xs) stmt)
evalS (Expression e) = evalE e >>| pure ()
evalS (Logical l) = evalL l >>| pure ()
evalS (s1 :. s2) = evalS s1 >>| evalS s2

class lit a :: a -> Expression a
class var a :: Ident -> a
class =. a where
	(=.) infixl 6 :: Ident a -> Stmt

instance lit Int where
	lit i = Elem bm i

instance lit [Int] where
	lit l = New bm l

instance var Elem where
	var i = Variable i

instance var Set where
	var i = Variable i

instance var Ident where
	var i = i

instance =. Elem where
	(=.) i e = Expression $ Att i e

instance =. Set where
	(=.) i s = Expression $ Att i s


element :: (Elem -> Elem)
element = id

// Start = 1
Start = eval $ "x" =. lit 4 :. "y" =. lit 5 :. "z" =. element (var "x")
  where
    eval e = id $ (unS (evalS e)) 'Map'.newMap