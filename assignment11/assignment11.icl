module assignment11 

import StdEnv
import StdDynamic
import Data.Map
import Data.Either
import qualified Data.List as List
from Data.Func import $

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

class expr v where
	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, * t
	(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	// (!=.) x y :== ~. (x == y)
	(>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	// (>.) x y :== y <. x
	(<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	// (<=.) x y :== ~. (y <. x)
	(>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	// (>=.) x y :== ~. (x <. y)
	(+=) infix 4 :: (v a p) (v [a] q) -> v [a] Expr 
	(-=) infix 4 :: (v a p) (v [a] q) -> v [a] Expr 
	If :: (v Bool p) (v a q) (v b r) -> v () Expr 
	While :: (v Bool p) (v a q) -> v () Expr
	(:.) infixr 1 :: (v a p) (v b q) -> v b Expr

class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t 
	var :: ((v t Upd) -> In t (v a p)) -> v a p | type t


// -------- Show --------

:: Show a p = Show (ShowState -> ShowState)
:: ShowState = { fresh :: Int,
				 ident :: Int,
				 print :: [String]}

instance zero ShowState where
	zero = {fresh = 0, ident = 0, print = []}

show :: a -> Show b c | toString a
show a = Show $ \s -> {s & print = [toString a:s.print]}

unShow :: (Show a b) -> ShowState -> ShowState
unShow (Show f) = f

(+.+) infixl 5 :: (Show a p) (Show b q) -> Show c r
(+.+) (Show f) (Show g) = Show (g o f)

fresh :: (Int -> (Show a p)) -> Show a p
fresh f = Show $ \s -> unShow (f s.fresh) {s & fresh = inc s.fresh}

freshVar :: ((Show b q) -> (Show a p)) -> (Show a p)
freshVar f = fresh (f o \n -> show ("v " +++ toString n))

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
	If b then else = show "if " +.+ b +.+ show " {" +.+ ident +.+ nl
		+.+ then +.+ unident +.+ nl +.+ show "} else {" +.+ ident +.+ nl
		+.+ else +.+ unident +.+ nl +.+ show "}" +.+ nl
	While b s = show "while " +.+ b +.+ show " {" +.+ ident +.+ nl
		+.+ s +.+ unident +.+ nl +.+ show "}" +.+ nl
	(:.) e1 e2 = e1 +.+ show ";" +.+ nl +.+ e2 +.+ nl

instance var Show where
	(=.) v e = v +.+ show " = " +.+ e
	var f = freshVar $ \v -> let (x In rest) = f v in
		show (type x) +.+ show " " +.+ v +.+ show " = " +.+
		show x +.+ show ";" +.+ nl +.+ rest


	/*	lit :: a -> v a Expr | type a
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, - t
	(*.) infixl 6 :: (v t p) (v t q) -> v t Expr | type, * t
	(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(!=.) x y :== ~. (x == y)
	(>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>.) x y :== y <. x
	(<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(<=.) x y :== ~. (y <. x)
	(>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>=.) x y :== ~. (x <. y)
	If :: (v Bool p) (v a q) (v b r) -> v () Expr 
	While :: (v Bool p) (v a q) -> v () Expr
	(:.) infixr 1 :: (v a p) (v b q) -> v b Expr*/




/*
(&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr
	(|.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr
	~. :: (v bool p) -> v Bool Expr
	(==.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(<.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(!=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | ==, type a
	(!=.) x y :== ~. (x == y)
	(>.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>.) x y :== y <. x
	(<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(<=.) x y :== ~. (y <. x)
	(>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr | <, Ord, type a
	(>=.) x y :== ~. (x <. y)
*/	

// -------- Eval --------

// :: Eval a p = Eval ((RW a) State -> (Either String a, State))
// :: RW a = R | W a
// :: State = { map :: Map Int Dynamic,
// 			 vars :: Int}

instance + [a] | == a where
	+ l1 l2 = 'List'.union l1 l2

instance - [a] | == a where
	- l1 l2 = 'List'.difference l1 l2

instance * [a] | == a where
	* l1 l2 = 'List'.intersect l1 l2

Start = 1