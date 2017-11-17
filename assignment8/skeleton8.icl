module skeleton8

/*
  Advanved Progrmming 2017, Assignment 8
  Pieter Koopman, pieter@cs.ru.nl
*/

from iTasks import class iTask, class toPrompt, class Publishable, instance Publishable Task,
	instance toPrompt String, instance Functor Task, 
	class TApplicative, instance TApplicative Task,
	generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, 
	:: JSONNode, :: TextFormat, :: Editor, :: TaskValue(..), :: Stability, :: Task, :: Action, 
	:: TaskCont(..), :: ViewOption(..), :: UpdateOption(..),
	-||-, -||, ||-, >>*, always, hasValue, updateInformation, viewInformation, startEngine
import qualified iTasks
import qualified iTasks.WF.Combinators.Overloaded as WF
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, StdClass, StdList, StdMaybe, StdString
import StdGeneric, StdBool, Data.Either
from StdFunc import o, flip
from Data.Func import $
from StdOrdList import sort
from StdOverloaded import class <
import qualified Data.List as List
import qualified Data.Map as Map

:: Expression = 
    New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     Set
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Logical = 
    TRUE | FALSE
  | (In) infix 4 Elem Set
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt = 
      Logical Logical
    | If Logical Stmt Stmt
    | For Ident Set Stmt
    | Expression Expression


:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String

// === State

:: Val = IntVal Int | SetVal [Int]
:: State :== 'Map'.Map String Val
:: Sem a = S (State -> Either String (a, State))

unS :: (Sem a) -> State -> Either String (a, State)
unS (S s) = s

instance Functor Sem where
  fmap f e = liftM f e

instance Applicative Sem where
  pure x = S $ \s -> Right(x, s)
  (<*>) fs ss = fs >>= 
      \f -> ss >>= 
      \s -> pure (f s)

instance Monad Sem where
  bind (S x) f = S $ \s -> case x s of
            (Right (v, s)) -> unS (f v) s
            (Left e) -> (Left e)

store :: Ident Val -> Sem Val
store i v = S $ \s -> Right (v, 'Map'.put i v s)

read :: Ident -> Sem Val
read i = S $ \s -> case 'Map'.get i s of
  Just v -> Right (v, s)
  Nothing -> Left ("Variable not found: " +++ i)

fail :: String -> Sem a
fail s = S $ \_ -> Left s

// === semantics

instance == Val where
  (==) (IntVal i1) (IntVal i2) = i1 == i2
  (==) (SetVal s1) (SetVal s2) = (sort s1) == (sort s2)

instance < Val where
  (<) (IntVal i1) (IntVal i2) = i1 < i2
  (<) _ _ = False

toList :: Val -> [Int]
toList (IntVal v) = [v]
toList (SetVal s) = s

semVal :: [Int] -> Sem Val
semVal l = pure $ SetVal l

eval :: Expression -> Sem Val
eval (New set) = pure (SetVal set)
eval (Elem e) = pure (IntVal e)
eval (Variable v) = read v
eval (Size set) = eval set
  >>= \v -> case v of
    (SetVal set) = pure (IntVal (length set))
    (IntVal _) = fail "Can't find the size of an integer"
eval (e1 +. e2) = eval e1 
    >>= \v1 -> eval e2 
    >>= \v2 -> case v1 of
      (IntVal i1) -> case v2 of
          (IntVal i2) -> pure $ IntVal $ i1 + i2
          (SetVal s) -> semVal $ 'List'.union s [i1]
      (SetVal s) -> semVal $ 'List'.union s (toList v2)
eval (e1 -. e2) = eval e1 
    >>= \v1 -> eval e2      // Not lazy
    >>= \v2 -> case v1 of
      (SetVal s1) -> semVal $  'List'.difference s1 (toList v2)
      (IntVal i1) -> case v2 of
        (IntVal i2) -> pure $ IntVal $ i1 + i2
        _ -> fail "Operator -. can't be used for Int,Set"
eval (e1 *. e2) = eval e1
    >>= \v1 -> eval e2      // Not lazy
    >>= \v2 -> case v1 of
      (IntVal i1) -> case v2 of
        (IntVal i2) -> pure $ IntVal $ i1 * i2
        (SetVal s) -> semVal $ map ((*)i1) s
      (SetVal s1) -> case v2 of
        (SetVal s2) -> semVal $ 'List'.intersect s1 s2
        _ -> fail "Oerator *. can't be used for Set,Int"
eval (i =. e) = eval e
  >>= \v -> store i v

evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (ee In se) = eval ee
  >>= \e -> eval se 
  >>= \s -> case e of
    (IntVal e) -> case s of
      (SetVal s) -> pure $ isMember e s
      _ -> fail "Operator In can't be used for Int,Int"
    _ -> fail "Operator In can't be used for Set,Set"
evalL (l1 ==. l2) = eval l1
  >>= \v1 -> eval l2
  >>= \v2 -> pure $ v1 == v2
evalL (e1 <=. e2) = eval e1
  >>= \v1 -> eval e2
  >>= \v2 -> pure $ v1 == v2
evalL (Not l) = evalL l
  >>= \v -> pure $ not v
evalL (l1 ||. l2) = evalL l1    // Sorry, McCarthy. I haven't been lazy, I'll keep consistency
  >>= \v1 -> evalL l2
  >>= \v2 -> pure $ v1 || v2
evalL (l1 &&. l2) = evalL l1    // Again, sorry, McCarthy
  >>= \v1 -> evalL l2
  >>= \v2 -> pure $ v1 && v2

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

Start = ()
