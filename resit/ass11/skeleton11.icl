module skeleton11

import StdMisc
import StdOverloaded
import StdDynamic
import StdString
import StdBool
// import Data.Functor
import qualified Data.Map as Map
import Data.Either
import Data.List
import Data.Maybe
// import Control.Applicative
// import Control.Monad
from Data.Func import $

:: Expr = Expr
:: Stmt = Stmt
:: Upd = Upd

class isExpr a where
	isExpr :: a -> a

class isStmt a where
	isStmt :: a -> a

instance isExpr Expr where
	isExpr a = a

instance isExpr Upd where
	isExpr a = a

instance isStmt Expr where
	isStmt a = a

instance isStmt Stmt where
	isStmt a = a

instance isStmt Upd where
	isStmt a = a

class expr v where
	set :: [a] -> v [a] Expr
	lit :: a -> v a Expr
	size :: (v [a] p) -> (v Int Expr)
	(+.) infixl 6 :: (v t p) (v t q) -> v t Expr | isExpr p & isExpr q & + t
	(-.) infixl 6 :: (v t p) (v t q) -> v t Expr | isExpr p & isExpr q & - t
	(*.) infixl 6 :: (v t p) (v t q) -> v t Expr | isExpr p & isExpr q & * t
	(+=) infixl 4 :: (v [t] p) (v t q) -> v [t] Expr | isExpr p & isExpr q & == t
	(-=) infixl 4 :: (v [t] p) (v t q) -> v [t] Expr | isExpr p & isExpr q & == t
	(&&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr | isExpr p & isExpr q
	(||.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr | isExpr p & isExpr q
	(==.) infixr 3 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & == t
	(<.) infix 4 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & < t
	~. :: (v Bool p) -> v Bool Expr | isExpr p 
	(>.) infix 4 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & < t
	(>.) x y :== y <. x
	(<=.) infix 4 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & < t
	(<=.) x y :== ~. (y <. x)
	(>=.) infix 4 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & < t
	(>=.) x y :== ~. (x <. y)
	(!=.) infix 4 :: (v t p) (v t q) -> v Bool Expr | isExpr p & isExpr q & == t
	(!=.) x y :== ~. (y ==. x)

class stmt v where
	If :: (v Bool p) (v t q) (v s r) -> v () Stmt | isExpr p
	While :: (v Bool p) (v t q) -> v () Stmt | isExpr p
	(:.) infixr 1 :: (v t p) (v s q) -> v s Stmt 

class var v where
	(=.) infixr 1 :: (v t Upd) (v t p) -> v t Expr | isExpr p
	var :: ((v t Upd) -> In t (v s p)) -> v s p | TC t

:: In a b = In infix 0 a b

// class type a | TC a where
// 	type :: a -> String

// instance type Bool where
// 	type _ = "Bool"

// instance type Int where
// 	type _ = "Int"

// instance type Char where
// 	type _ = "Char"

:: Eval a b = Eval ((RW a) State -> (Either String a, State))
:: RW a = R | W a
:: State = { map :: 'Map'.Map Int Dynamic, vars :: Int}

rwvar :: Int (RW a) State -> (Either String a, State) | TC a
rwvar i (W a) s = let s = {s & map = 'Map'.put i (dynamic a) s.map,
                               vars = inc s.vars}
                  in (Right a, s)
rwvar i R s = case 'Map'.get i s.map of
	Just (a :: a^) = (Right a,s)
	Just (x :: a) = (Left $ "Type mismatch: v" +++ toString i, s)
	Nothing = (Left $ "Variable not found: v" +++ toString i, s)

unE :: (Eval a b) (RW a) -> (State -> (Either String a, State))
unE (Eval e) r = e r

fmap :: (a -> b) (Eval a c) -> Eval b d 
fmap f (Eval e) = Eval \_ s -> case e R s of
		(Right a,s) = (Right (f a),s)
		(Left e,s) = (Left e,s)

pure :: a -> Eval a b
pure a = Eval \r s -> (Right a, s)

(<*>) infixl 4 :: (Eval (a -> b) p) (Eval a q) -> Eval b r
(<*>) (Eval ef) (Eval ea) = Eval \r s -> case ef R s of
	(Right f,s) = case ea R s of
		(Right a,s) = (Right (f a),s)
		(Left e,s) = (Left e,s)
	(Left e,s) = (Left e,s)

(>>=) infixl 1 :: (Eval a p) (a -> Eval b q) -> Eval b q
(>>=) (Eval em) f = Eval \r s -> case em R s of
	(Right m,s) = unE (f m) r s
	(Left e,s) = (Left e,s)

// (>>|) infixl 1 :: (Evla a p) (Eval b q) -> Eval b q
(>>|) a b :== a >>= \_ -> b

instance expr Eval where
	set s = pure s
	lit x = pure x
	size l = pure length <*> l
	(+.) e1 e2 = pure (+) <*> e1 <*> e2
	(-.) e1 e2 = pure (-) <*> e1 <*> e2
	(*.) e1 e2 = pure (*) <*> e1 <*> e2
	(+=) l e = pure union <*> l <*> fmap (\x -> [x]) e
	(-=) l e = pure difference <*> l <*> fmap (\x -> [x]) e
	(&&.) e1 e2 = pure (&&) <*> e1 <*> e2
	(||.) e1 e2 = pure (||) <*> e1 <*> e2
	(==.) e1 e2 = pure (==) <*> e1 <*> e2
	(~.) e = fmap not e
	(<.) e1 e2 = pure (<) <*> e1 <*> e2

instance stmt Eval where
	If c t e = c >>= \c -> if c (t >>| pure ()) (e >>| pure ())
	While ce s = ce >>= \c -> if c (s >>| While ce s) (pure ())
	(:.) s1 s2 = toStmt s1 >>| toStmt s2 

instance var Eval where
	(=.) (Eval u) e = e >>= \v -> Eval \_ s -> u (W v) s
	var f = Eval \r s -> let (t In (Eval rest)) = f (Eval (rwvar s.vars))
	                     in rest R {s & vars = inc s.vars,
	                                    map = 'Map'.put s.vars (dynamic t) s.map}

toStmt :: (Eval t p) -> Eval t Stmt
toStmt (Eval s) = Eval s 

Start = 1