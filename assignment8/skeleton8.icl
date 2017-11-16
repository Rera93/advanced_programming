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
import StdGeneric, StdBool
from StdFunc import o
from Data.Func import $
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
:: Sem a = S (State -> (a, State))
:: Sema a :== State -> (a, State)

unS :: (Sem a) -> State -> (a, State)
unS (S s) = s

instance Functor Sem where
  fmap f (S e) = S $ \s -> let (v,s) = e s in (f v, s)

instance Applicative Sem where
  pure x = S $ \s -> (x, s)
  (<*>) (S fs) (S ss) = S f
    where
      f s 
        # (f, s) = fs s
          (v, s) = ss s
        = (f v, s)

instance Monad Sem where
  bind (S x) f = S $ \s -> let (v, s) = x s in (unS (f v) s)

// === semantics


// === simulation
(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew

Start = ()
