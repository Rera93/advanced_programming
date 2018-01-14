// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module mcuSim

import mcuSim
import Data.Eq
import qualified Data.List as List

instance zero State where
	zero = {map = newMap, vars = 0, buttons = newMap}

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
rwvar n R s = case get n s.map of
	Just (v :: a^) = (Right v, s)
	Just _ = (Left (toString n + " wrong type"), s)
	_ = (Left (toString n + " undefined"), s)
rwvar n (W a) s = (Right a, { s & map = put n (dynamic a) s.map })

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
	// I didn't have time to figure the for evaluation. My attempt is commented below.
	For s f = s >>- \set -> rtrn ()
	// For s f = Eval \r st -> let (x In (Eval rest)) = f (Eval (rwvar s.vars)) in 
	// 							case unEval s R st of
	// 								(Right set, ns) = foldl (\s e -> x =. e :. rest) (Right (), ns) set
	// 								(Left e, _) = (Left e, st)
	(:.) e1 e2 = e1 >>- \_ -> toExpr e2
		where
			toExpr :: (Eval t p) -> Eval t Expr
			toExpr (Eval f) = Eval f

instance var Eval where
	(=.) v e = e >>- \a -> Eval \r s -> unEval v (W a) s
	var f = Eval \r s -> let (x In (Eval rest)) = f (Eval (rwvar s.vars))
						 in rest R {s & vars = inc s.vars,
									map = put s.vars (dynamic x) s.map}

instance button Eval where
	isPressed b = Eval \_ s -> case get b s.buttons of
		Nothing = (Left ("Can't find button " + toString b), s)
		Just v = (Right v, s)

eval :: (Eval a p) -> [String] | type a
eval (Eval f) = let (r, s) = f R zero in
		case r of
			Right a = [toString a, printState s]
			Left e = ["Error:",e,"\n"]
	where
		printState :: State -> String
		printState {map} = foldrWithKey (\k v a -> a + printVar k v) "" map
		printVar :: Int Dynamic -> String 
		printVar k (v :: Int) = "(" + toString k + ":" + toString v + ")"
		printVar k (v :: Char) = "(" + toString k + ":" + toString v + ")"
		printVar k (v :: Bool) = "(" + toString k + ":" + toString v + ")"
		printVar k (v :: [Int]) = "(" + toString k + ":" + print v + ")"	// List's instance of toString isn't the best
		printVar k (v :: [Char]) = "(" + toString k + ":" + print v + ")"
		printVar k (v :: [Bool]) = "(" + toString k + ":" + print v + ")"
		printVar k (v :: a) = "unknown type variable"