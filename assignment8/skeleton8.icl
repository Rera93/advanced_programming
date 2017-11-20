// Matheus Amazonas Cabral de Andrade
// s4605640

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
	-||-, -||, ||-, >>*, always, hasValue, updateInformation, viewInformation, startEngine, enterInformation,
  :: EnterOption, defaultValue, @!, Action
import qualified iTasks
import qualified iTasks.WF.Combinators.Overloaded as WF
import Data.Maybe
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, StdClass, StdList, StdString
import StdGeneric, StdBool, Data.Either
from StdFunc import o, flip
from Data.Func import $
from StdOrdList import sort
from StdOverloaded import class <
from StdTuple import snd
import qualified Data.List as List
import qualified Data.Map as Map
import StdEnum
import GenPrint

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
    | Expression Expression
    | If Logical Stmt Stmt
    | For Ident Set Stmt
    
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

// I'm not sure whether statements should wreturn a value and what's the desired relation betweem
// statements and expresssions, but the data type contains constructor for Logical and Expression,
// wo I must return one of those.
:: StmtVal = Exp Val | Log Bool

evalS :: Stmt -> Sem StmtVal
evalS (Logical l) = evalL l
  >>= \v -> pure $ Log v
evalS (Expression e) = eval e
  >>= \v -> pure $ Exp  v
evalS (If l s1 s2) = evalL l
  >>= \v -> if v (evalS s1) (evalS s2)
evalS (For i set stmt) = eval set
  >>= \values -> case values of
    (SetVal set) -> forEach i set stmt
    (IntVal v) -> forEach i [0..v] stmt
  where
    forEach :: Ident [Int] Stmt -> Sem StmtVal
    forEach i set stmt = foldr seqStmt (pure (Log True)) (map (exec stmt i) set)
    seqStmt :: (Sem StmtVal) (Sem StmtVal) -> Sem StmtVal
    seqStmt (S f) (S g) = S $ \x -> f x >>= \(_,s) -> g s   // We don't care about the value, just change the state
    exec :: Stmt Ident Int -> Sem StmtVal
    exec stmt i v = store i (IntVal v) >>= \_ -> evalS stmt

// === printing

class printable a where
  print :: a -> String

instance printable Ident where
  print i = i

instance printable Expression where
  print (New l) = printToString l
  print (Elem i) = printToString i
  print (Variable v) = v
  print (Size set) = "sizeOf " +++ print set
  print (e1 +. e2) = print e1 +++ " + " +++ print e2
  print (e1 -. e2) = print e1 +++ " - " +++ print e2
  print (e1 *. e2) = print e1 +++ " * " +++ print e2
  print (e1 =. e2) = print e1 +++ " = " +++ print e2

instance printable Logical where
  print TRUE = "True"
  print FALSE = "False"
  print (e In s) = print e +++ " in " +++ print s
  print (e1 ==. e2) = print e1 +++ " == " +++ print e2
  print (e1 <=. e2) = print e1 +++ " <= " +++ print e2
  print (Not l) = "not " +++ print l
  print (l1 ||. l2) = print l1 +++ " || " +++ print l2
  print (l1 &&. l2) = print l1 +++ " && " +++ print l2

instance printable Stmt where
  print (Logical l) = print l
  print (Expression e) = print e
  print (If p t e) = "if " +++ print p +++ "then\n\t" +++ print t +++ "\n\telse\n\t" +++ print e
  print (For i set stmt) = "for " +++ i +++ " in " +++ print set +++ " do " +++ print stmt

// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

derive class iTask Expression, Logical, Stmt, Sem, Val, StmtVal
derive gPrint Val, StmtVal, (,)

createProgram :: Task State
createProgram 
    # s =  'Map'.newMap
    = enterInformation "Enter the first input" [] 
      >>>= loop s

loop :: State Stmt -> Task State
loop state stmt = case (unS (evalS stmt)) state of
    (Left e) = viewInformation "Evaluation error" [] e
          >>>| loop state (Logical TRUE)
    (Right (v, state)) = (enterInformation "Enter a new statement" []
          -|| viewInformation "Value" [] (printToString v)
          -|| viewInformation "State" [ViewAs (printToString o 'Map'.toList)] state
          -|| viewInformation "Pretty print" [] (print stmt))
          >>* [OnAction ActionOk (hasValue (loop state)),
               OnAction (Action "Reset state") (always (loop ('Map'.newMap) stmt)),
               OnAction (Action "Quit") (always createProgram)]

Start :: *World -> *World
Start w = startEngine createProgram w
