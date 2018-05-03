module skeleton8

/*
  Advanved Progrmming 2017, Assignment 8
  Pieter Koopman, pieter@cs.ru.nl
*/

from iTasks import class iTask, class toPrompt,
	instance toPrompt String, instance Functor Task, 
	class TApplicative, instance TApplicative Task,
	generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, 
	:: JSONNode, :: TextFormat, :: Editor, :: TaskValue(..), :: Stability, :: Task, :: Action, 
	:: TaskCont(..), :: ViewOption(..), :: UpdateOption(..), :: EnterOption(..),
	-||-, -||, ||-, >>*, always, hasValue, updateInformation, viewInformation, startEngine, enterInformation
from iTasks.Engine import class Publishable, instance Publishable (Task a)
import qualified iTasks
import qualified iTasks.WF.Combinators.Overloaded as WF
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, StdClass, StdList, Data.Maybe, StdString, Data.Either
import StdGeneric, StdBool
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map
from Data.Func import $
import StdMisc

:: Expression
  = New      [Int]
	| Elem     Int
	| Variable Ident
	| Size     Set
	| (+.) infixl 6 Expression Expression
	| (-.) infixl 6 Expression Expression
	| (*.) infixl 7 Expression Expression
	| (=.) infixl 2 Ident Expression

:: Logical
	= TRUE | FALSE
	| (In) infix 4 Elem Set
	| (==.) infix 4 Expression Expression
	| (<=.) infix 4 Expression Expression
	| Not Logical
	| (||.) infixr 2 Logical Logical
	| (&&.) infixr 3 Logical Logical

:: Stmt
	= If Logical Stmt Stmt
	| For Ident Set Stmt
	| Expression Expression
	| Logical Logical

:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String

// === State

:: Val = IntV Int | SetV [Int]
:: StmtVal = Val Val | Bool Bool
:: State :== 'Map'.Map Ident Val

:: Sem a = S (State -> Either String (a, State))

emptyState :: State
emptyState = 'Map'.newMap

unS :: (Sem a) -> (State -> Either String (a, State))
unS (S s) = s

instance == Val where
	(==) (IntV i1) (IntV i2) = i1 == i2
	(==) (SetV s1) (SetV s2) = s1 == s2
	(==) _ _ = False

instance Functor Sem where
	fmap f (S p) = S \s -> case p s of
		Right (a,s) = Right (f a, s)
		Left e = Left e

instance Applicative Sem where
	pure a = S \s -> Right (a,s)
	(<*>) (S sf) (S sa) = S \s -> case sf s of
		Right (f,s) = case sa s of
			Right (a,s) = Right (f a, s)
			Left e = Left e
		Left e = Left e

instance Monad Sem where
	bind (S ma) f = S \s -> case ma s of
		Right (a,s) = unS (f a) s
		Left e = Left e

store :: Ident Val -> Sem Val
store i v = S \s -> Right (v, 'Map'.put i v s)

read :: Ident -> Sem Val
read i = S \s -> case 'Map'.get i s of
	Just v = Right (v,s)
	Nothing = Left $ "Variable not found: " +++ i

fail :: String -> Sem a
fail e = S \_ -> Left e

// === semantics

toList :: Val -> [Int]
toList (IntV x) = [x]
toLsit (SetV s) = s

eval :: Expression -> Sem Val
eval (New set) = pure (SetV set)
eval (Elem e) = pure (IntV e)
eval (Variable i) = read i
eval (Size s) = eval s >>= \(SetV s) -> pure $ IntV $ length s
eval (e1 +. e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> case v1 of
	(IntV x1) = case v2 of
		(IntV x2) = pure $ IntV $ x1 + x2
		(SetV s2) = pure $ SetV $ 'List'.union [x1] s2
	(SetV s1) = pure $ SetV $ 'List'.union s1 (toList v2)
eval (e1 -. e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> case v1 of
	(IntV x1) = case v2 of
		(IntV x2) = pure $ IntV $ x1 - x2
		(SetV s2) = fail "Error: Operator -. used in (Int,Set)."
	(SetV s1) = pure $ SetV $ 'List'.difference s1 (toList v2)
eval (e1 *. e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> case v1 of
	(IntV x1) = case v2 of
		(IntV x2) = pure $ IntV $ x1 * x2
		(SetV s2) = pure $ SetV $ map ((*)x1) s2
	(SetV s1) = case v2 of
		(IntV x2) = fail "Error: Operator *. used in (Set,Int)."
		(SetV s2) = pure $ SetV $ 'List'.intersect s1 s2

evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (ee In se) = eval ee >>= \e -> case e of
	(IntV e) = eval se >>= \s -> case s of
		(SetV s) = pure $ isMember e s
		_ = fail "Error: Operator In used in (Int,Int)."
	_ = fail "Error: Operator In used in (Set,Set)."
evalL (e1 ==. e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> pure $ v1 == v2
evalL (e1 <=. e2) = eval e1 >>= \v1 -> eval e2 >>= \v2 -> case v1 of
	(IntV x1) = case v2 of
		(IntV x2) = pure $ x1 < x2
		_ = fail "Error: Operator <. used in (Int,Set)."
	_ = fail "Error: Operator <. used in Sets."
evalL (Not l) = evalL l >>= \b -> pure $ not b
evalL (l1 ||. l2) = evalL l1 >>= \v1 -> evalL l2 >>= \v2 -> pure $ v1 || v2
evalL (l1 &&. l2) = evalL l1 >>= \v1 -> evalL l2 >>= \v2 -> pure $ v1 && v2

evalS :: Stmt -> Sem StmtVal
evalS (If c t e) = evalL c >>= \c -> if c (evalS t) (evalS e)
evalS (Expression e) = eval e >>= pure o Val
evalS (Logical l) = evalL l >>= pure o Bool
evalS (For i setE stmt) = eval setE >>= \set -> case set of
	(SetV []) = pure $ Bool True
	(SetV [x:xs]) = store i (IntV x) >>| evalS stmt >>| evalS (For i (New xs) stmt) 

// === Printing
class print a :: a -> String

instance print Expression where
	print (New set) = toString set
	print (Elem e) = toString e
	print (Variable i) = i
	print (Size s) = "sizeOf " +++ print s
	print (e1 +. e2) = print e1 +++ " + " +++ print e2
	print (e1 -. e2) = print e1 +++ " - " +++ print e2
	print (e1 *. e2) = print e1 +++ " * " +++ print e2

instance print Logical where
	print TRUE = "True"
	print FALSE = "False"
	print (ee In se) = print ee +++ " in " +++ print se
	print (e1 ==. e2) = print e1+++ "== " +++ print e2
	print (e1 <=. e2) = print e1+++ "<= " +++ print e2
	print (Not l) = "not " +++ print l
	print (l1 ||. l2) = print l1+++ "|| " +++ print l2
	print (l1 &&. l2) = print l1+++ "&& " +++ print l2

instance print Stmt where
	print (If c t e) = "if " +++ print c +++ " then " +++ print t +++ " else " +++ print e
	print (Expression e) = print e
	print (Logical l) = print l
	print (For i setE stmt) = "for " +++ i +++ " in " +++ print setE +++ " do " +++ print stmt

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

derive class iTask Expression, Val

mainTask = enterInformation "Enter an expression" []
	>>* [OnAction ActionOk (hasValue display),
		OnAction ActionQuit (always mainTask)]
where
	display :: Expression -> Task String
	display e = case unS (eval e) emptyState of
		Right (v,s) = viewInformation "Value: " [] v ||-
			viewInformation "State: " [] s ||-
			viewInformation "Print: " [] (print e) 
		Left e = viewInformation "Error: " [] e

Start w = startEngine mainTask w
