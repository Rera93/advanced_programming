module assignment11 

import StdString
import StdEnv
import StdDynamic
import Data.Map
import Data.Either
import Data.Eq
import Data.Tree
import GenPrint
import qualified Data.List as List
from Data.Func import $
from Data.Foldable import concat, class Foldable

:: Upd = Upd
:: Expr = Expr
:: In a b = In infix 0 a b

class type a | toString, TC a where
	type :: a -> String

instance type Bool where
	type _ = "bool"

instance type Int where
	type _ = "int"

instance type Char where
	type _ = "char"

instance type [a] | type a where
	type a = "[" +++ type a +++ "]"

class expr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, * t
	(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v Bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(+=) infix 4 :: (v a p) (v [a] q) -> v [a] Expr | == a
	(-=) infix 4 :: (v a p) (v [a] q) -> v [a] Expr | == a
	sizeOf :: (v [a] p) -> v Int Expr
	If :: (v Bool p) (v a q) (v b r) -> v () Expr 
	// For :: (In (v t Upd) (v [t] q)) (v a Expr) -> v () Expr
	(:.) infixr 1 :: (v a p) (v b q) -> v b Expr

class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t 
	var :: ((v t Upd) -> In t (v a p)) -> v a p | type t

print :: [a] -> String | toString a 
print l = "[" + show l + "]"
	where
		show :: [a] -> String | toString a
		show [] = ""
		show [a] = toString a
		show [a:as] = toString a + "," + show as

// -------- Show --------

:: Show a p = Show (ShowState -> ShowState)
:: ShowState = { fresh :: Int,
				 ident :: Int,
				 print :: [String]}

instance zero ShowState where
	zero = {fresh = 0, ident = 0, print = []}

instance + String where
	+ s1 s2 = s1 +++ s2

show :: a -> Show b c | toString a
show a = Show $ \s -> {s & print = [toString a:s.print]}

unShow :: (Show a b) -> ShowState -> ShowState
unShow (Show f) = f

(+.+) infixl 5 :: (Show a p) (Show b q) -> Show c r
(+.+) (Show f) (Show g) = Show (g o f)

fresh :: (Int -> (Show a p)) -> Show a p
fresh f = Show $ \s -> unShow (f s.fresh) {s & fresh = inc s.fresh}

freshVar :: ((Show b q) -> (Show a p)) -> (Show a p)
freshVar f = fresh (f o \n -> show ("v" + toString n))

ident :: Show a b
ident = Show $ \s -> {s & ident = inc s.ident}

unident :: Show a b
unident = Show $ \s -> {s & ident = s.ident-1}

nl :: Show a b
nl = Show $ \s -> {s & print = [toString ['\n':repeatn (2 * s.ident) ' ']:s.print]}

brac :: (Show a p) -> (Show b q)
brac e = show "(" +.+ e +.+ show ")"

instance expr Show where
	lit a = show a
	(+.) x y = brac $ x +.+ show "+" +.+ y
	(-.) x y = brac $ x +.+ show "-" +.+ y
	(*.) x y = brac $ x +.+ show "*" +.+ y
	(&.) x y = brac $ x +.+ show "&" +.+ y
	(|.) x y = brac $ x +.+ show "|" +.+ y
	(~.) x = brac $ show "~" +.+ x
	(==.) x y = brac $ x +.+ show "==" +.+ y
	(!=.) x y = brac $ x +.+ show "!=" +.+ y
	(>.) x y = brac $ x +.+ show ">" +.+ y
	(<.) x y = brac $ x +.+ show "<" +.+ y
	(>=.) x y = brac $ x +.+ show ">=" +.+ y
	(<=.) x y = brac $ x +.+ show "<=" +.+ y
	(+=) e s = e +.+ show " += " +.+ s
	(-=) e s = e +.+ show " -= " +.+ s
	sizeOf l = show "sizeOf(" +.+ l +.+ show ")"
	If b then else = show "if " +.+ b +.+ show " {" +.+ ident +.+ nl
		+.+ then +.+ unident +.+ nl +.+ show "} else {" +.+ ident +.+ nl
		+.+ else +.+ unident +.+ nl +.+ show "}" +.+ nl
	// For (In x s) e = show "for"
		// freshVar $ \v -> let (x in rest) = f v
		// show "for " +.+ v +.+ show " in " +.+ s +.+ show "do {"
		// +.+ ident +.+ nl +.+ e +.+ unident +.+ nl +.+ show "}" +.+ nl
	(:.) e1 e2 = e1 +.+ show ";" +.+ nl +.+ e2 +.+ nl

instance var Show where
	(=.) v e = v +.+ show " = " +.+ e
	var f = freshVar $ \v -> let (x In rest) = f v in
		show (type x) +.+ show " " +.+ v +.+ show " = " +.+
		show x +.+ show ";" +.+ nl +.+ rest

// -------- Eval --------

:: Eval a p = Eval ((RW a) State -> (Either String a, State))
:: RW a = R | W a
:: State = { map :: Map Int Dynamic,
			 vars :: Int}

instance zero State where
	zero = {map = newMap, vars = 0}

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
	// For s e = show "for"
	// 	// freshVar $ \v -> let (x in rest) = f v
	// 	// show "for " +.+ v +.+ show " in " +.+ s +.+ show "do {"
	// 	// +.+ ident +.+ nl +.+ e +.+ unident +.+ nl +.+ show "}" +.+ nl
	(:.) e1 e2 = e1 >>- \_ -> toExpr e2
		where
			toExpr :: (Eval t p) -> Eval t Expr
			toExpr (Eval f) = Eval f

instance var Eval where
	(=.) v e = e >>- \a -> Eval \r s -> unEval v (W a) s
	var f = Eval \r s -> let (x In (Eval rest)) = f (Eval (rwvar s.vars))
						 in rest R {s & vars = inc s.vars,
									map = put s.vars (dynamic x) s.map}

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

test = 
	var \e = True In
	var \s = False In
	var \o = [1,2,3] In
	e 

Start = eval test









