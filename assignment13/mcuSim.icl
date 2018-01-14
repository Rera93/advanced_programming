// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module mcuSim

import mcuSim
import Data.Eq
import iTasks
import qualified Data.List as List
from Data.Func import $

derive class iTask State, Button

instance zero State where
	zero = {map = [(0,dynamic 2)], vars = 0, buttons = [(B1,False), (B2,False), (B3,False), (B4,False), (B5, False)]}

unEval :: (Eval a p) -> (RW a) State -> (Either String a, State)
unEval (Eval f) = f

instance + [a] | == a where
	+ l1 l2 = 'List'.union l1 l2

instance - [a] | == a where
	- l1 l2 = 'List'.difference l1 l2

instance * [a] | == a where
	* l1 l2 = 'List'.intersect l1 l2

rtrn :: a -> Eval a p
rtrn a = Eval \r s -> (Right a, s)

(>>-) infixl 1 :: (Eval a p) (a -> Eval b q) -> Eval b q
(>>-) (Eval e) f = Eval \r s -> case e R s of
    (Right a,t) = unEval (f a) r t
    (Left e,t) = (Left e,t)

(<*.>) infixl 4 :: (Eval (a -> b) p) (Eval a q) -> Eval b r 
(<*.>) f a = f >>- \g -> a >>- \b -> rtrn (g b)

rwvar :: Int (RW a) State -> (Either String a, State) | TC a
rwvar n R s = case getM n s.map of
	Just (v :: a^) = (Right v, s)
	Just _ = (Left (toString n + " wrong type"), s)
	_ = (Left (toString n + " undefined"), s)
rwvar n (W a) s = (Right a, { s & map = putM n (dynamic a) s.map })

instance expr Eval where
	lit a = rtrn a
	(+.) x y = rtrn (+) <*.> x <*.> y
	(-.) x y = rtrn (-) <*.> x <*.> y
	(*.) x y = rtrn (*) <*.> x <*.> y
	(&.) x y = rtrn (&&) <*.> x <*.> y
	(|.) x y = rtrn (||) <*.> x <*.> y
	(~.) x = rtrn not <*.> x
	(==.) x y = rtrn (==) <*.> x <*.> y
	(!=.) x y = rtrn (/=) <*.> x <*.> y
	(>.) x y = rtrn (>) <*.> x <*.> y
	(<.) x y = rtrn (<) <*.> x <*.> y
	(>=.) x y = rtrn (>=) <*.> x <*.> y
	(<=.) x y = rtrn (<=) <*.> x <*.> y
	(+=) e s = rtrn (\x y -> 'List'.union x [y]) <*.> s <*.> e
	(-=) e s = rtrn (\x y -> 'List'.difference x [y]) <*.> s <*.> e
	sizeOf l = rtrn length <*.> l
	If b then else = b >>- \c -> if c 
		(then >>- \_ -> rtrn ())
		(else >>- \_ -> rtrn ())
	(:.) e1 e2 = e1 >>- \_ -> toExpr e2
		where
			toExpr :: (Eval t p) -> Eval t Expr
			toExpr (Eval f) = Eval f

instance var Eval where
	(=.) v e = e >>- \a -> Eval \r s -> unEval v (W a) s
	var f = Eval \r s -> let (x In (Eval rest)) = f (Eval (rwvar s.vars))
						 in rest R {s & vars = inc s.vars,
									map = putM s.vars (dynamic x) s.map}
	global f = Eval \r s -> let (x In (Eval rest)) = f (Eval (rwvar s.vars))
						 in rest R {s & vars = inc s.vars,
									map = putM s.vars (dynamic x) s.map}

instance button Eval where
	isPressed b = Eval \_ s -> case getM b s.buttons of
		Nothing = (Left ("Can't find button " + toString b), s)
		Just v = (Right v, s)
	pressed b = Eval \_ s -> case getM b s.buttons of
		Nothing = (Left ("Can't find button " + toString b), s)
		Just _ = (Right b, {s & buttons = putM b False s.buttons})

print :: [a] -> String | toString a 
print l = "[" + show l + "]"
	where
		show :: [a] -> String | toString a
		show [] = ""
		show [a] = toString a
		show [a:as] = toString a + "," + show as

eval :: (Eval a p) -> State | type a
eval (Eval f) = snd $ f R zero 


