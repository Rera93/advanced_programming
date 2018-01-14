// Matheus Amazonas Cabral de Andrade
// s4605640

definition module mcu

import StdEnv

:: Upd = Upd
:: Expr = Expr
:: In a b = In infix 0 a b

:: Button = B1 | B2 | B3 | B4 | B5

class type a | toString, TC a where
	type :: a -> String

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
	For :: (v [t] q) ((v t Upd) -> In t (v a p)) -> v () Expr | type t
	(:.) infixr 1 :: (v a p) (v b q) -> v b Expr

class var v where
	(=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr | type t 
	var :: ((v t Upd) -> In t (v a p)) -> v a p | type t

class button v where
	isPressed :: Button -> v Bool Expr

instance toString Button 
instance < Button 
instance type Bool
instance type Int
instance type Char
instance type [a] | type a
instance type ()
instance toString ()
instance + String 

print :: [a] -> String | toString a 