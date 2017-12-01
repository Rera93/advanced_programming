// Matheus Amazonas Cabral de Andrade
// s4605640

module assignment10

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import StdBool
import StdEnv
import StdFunc
import StdList 
import StdString
import StdTuple
import GenPrint
import StdDynamic
from Data.Func import $
import qualified Data.Map as Map

// ----- Bimap -----

:: BM a b = { t :: a -> b, f :: b -> a}

bm :: BM a a
bm = {t = id, f = id}

// ----- DSL Syntax -----
:: Set    :== Expression [Int]
:: Elem  :== Expression Int
:: Ident  :== String

:: Expression a = 
    New       (BM a [Int]) [Int]
  | Elem      (BM a Int)   Int
  | VarElem   (BM a Int)   String
  | VarSet    (BM a [Int]) String
  | Size      (BM a Int)   Set
  | Plus 	    (BM a Int)   Elem Elem
  | Union     (BM a [Int]) Set Set
  | AddEleSet (BM a [Int]) Elem Set
  | AddSetEle (BM a [Int]) Set Elem
  | Minus     (BM a Int)   Elem Elem
  | Diff	    (BM a [Int]) Set Set 
  | DiffE     (BM a [Int]) Set Elem
  | Mult	    (BM a Int)   Elem Elem
  | Inter 	  (BM a [Int]) Set Set
  | Scale	    (BM a [Int]) Elem Set
  | AttElem   (BM a Int)   Ident Elem
  | AttSet    (BM a [Int]) Ident Set

:: Logical = 
    TRUE 
  | FALSE 
  | (In) infix 4 Elem Set
  | E.a: (==.) infix 4 (Expression a) (Expression a) & == a
  | (<=.) infix 4 Elem Elem
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt = 
    Logical Logical
  | E.a: Expression (Expression a)
  | If Logical Stmt Stmt
  | For Ident Set Stmt
  | (:.) infixl 1 Stmt Stmt

// ----- State -----

:: State :== 'Map'.Map String Dynamic
:: Sem a = S (State -> (a, State)) 

unS :: (Sem a) -> State -> (a, State)
unS (S s) = s

instance Functor Sem where
  fmap f e = liftM f e

instance Applicative Sem where
  pure x = S $ \s -> (x, s)
  (<*>) fs ss = fs >>= 
      \f -> ss >>= 
      \s -> pure (f s)

instance Monad Sem where
  bind (S x) f = S $ \s -> let (v,ns) = x s in unS (f v) ns

store :: Ident a (BM b a) -> Sem b | TC a
store i v bm = S $ \s -> (bm.f v, 'Map'.put i (dynamic v) s)

read :: Ident -> Sem a | TC a
read i = S $ \s -> case 'Map'.get i s of
    Just (v :: a^) = (v, s)
    Just d = abort $ "Expected " +++ toString expType +++ " but got " +++ toString (typeCodeOfDynamic d)
    Nothing = abort $ "Variable not found: " +++ i
  where
    expType = typeCodeOfDynamic (dynamic undef :: a^)

// ----- Evaluation -----

evalE :: (Expression a) -> Sem a
evalE (New bm set) = pure $ bm.f set
evalE (Elem bm ele) = pure $ bm.f ele
evalE (VarElem bm var) = read var >>= \v -> pure $ bm.f v
evalE (VarSet bm var) = read var >>= \v -> pure $ bm.f v
evalE (Size bm eSet) = evalE eSet >>= \set -> pure $ bm.f $ length set
evalE (Plus bm e1 e2) = evalE e1 >>= \x1 -> evalE e2 >>= \x2 -> pure $ bm.f $ x1+x2
evalE (Union bm e1 e2) = evalE e1 >>= \s1 -> evalE e2 >>= \s2 -> pure $ bm.f $ union s1 s2
evalE (AddEleSet bm ee se) = evalE ee >>= \e -> evalE se >>= \s -> pure $ bm.f $ union s [e]
evalE (AddSetEle bm se ee) = evalE ee >>= \e -> evalE se >>= \s -> pure $ bm.f $ union s [e]
evalE (Minus bm e1 e2) = evalE e1 >>= \x1 -> evalE e2 >>= \x2 -> pure $ bm.f $ x1-x2
evalE (Diff bm e1 e2) = evalE e1 >>= \s1 -> evalE e2 >>= \s2 -> pure $ bm.f $ difference s1 s2
evalE (DiffE bm e1 e2) = evalE e1 >>= \s -> evalE e2 >>= \e -> pure $ bm.f $ difference s [e]
evalE (Mult bm e1 e2) = evalE e1 >>= \x1 -> evalE e2 >>= \x2 -> pure $ bm.f $ x1*x2
evalE (Inter bm e1 e2) = evalE e1 >>= \s1 -> evalE e2 >>= \s2 -> pure $ bm.f $ intersect s1 s2
evalE (Scale bm ee se) = evalE ee >>= \e -> evalE se >>= \s -> pure $ bm.f $ map ((*)e) s
evalE (AttElem bm i ee) = evalE ee >>= \e -> store i e bm
evalE (AttSet bm i se) = evalE se >>= \s -> store i s bm

evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (ee In se) = evalE ee >>= \e -> evalE se >>= \s -> pure $ isMember e s
evalL (e1 ==. e2) = evalE e1 >>= \v1 -> evalE e2 >>= \v2 -> pure $ v1 == v2
evalL (e1 <=. e2) = evalE e1 >>= \v1 -> evalE e2 >>= \v2 -> pure $ v1<=v2
evalL (Not e) = evalL e >>= \l -> pure $ not l
evalL (e1 ||. e2) = evalL e1 >>= \b1 -> evalL e2 >>= \b2 -> pure $ b1 || b2
evalL (e1 &&. e2) = evalL e1 >>= \b1 -> evalL e2 >>= \b2 -> pure $ b1 && b2

evalS :: Stmt -> Sem ()
evalS (Logical _) = pure () // I don't understand why there's a Logical constructor for Stmt
evalS (Expression e) = evalE e >>| pure ()
evalS (If p s1 s2) = evalL p >>= \b -> if b (evalS s1) (evalS s2) >>| pure ()
evalS (es1 :. es2) = evalS es1 >>| evalS es2 >>| pure ()
evalS (For i eset stmt) = evalE eset >>= \set -> forEach i set stmt
  where
    forEach :: Ident [Int] Stmt -> Sem ()
    forEach i set stmt = foldr (>>|) (pure ()) (map (exec stmt i) set)
    exec :: Stmt Ident Int -> Sem ()
    exec stmt i v = store i v bm >>= \_ -> evalS stmt

// ----- Printing -----

class printable a where
  print :: a -> String

instance printable Ident where
  print i = i

instance printable (Expression a) where
  print (New _ l) = printToString l
  print (Elem _ i) = printToString i
  print (VarElem _ v) = v
  print (VarSet _ v) = v
  print (Size _ set) = "sizeOf " +++ print set
  print (Plus _ e1 e2) = print e1 +++ " + " +++ print e2
  print (Union _ e1 e2) = print e1 +++ " + " +++ print e2
  print (AddEleSet _ e1 e2) = print e1 +++ " + " +++ print e2
  print (AddSetEle _ e1 e2) = print e1 +++ " + " +++ print e2
  print (Minus _ e1 e2) = print e1 +++ " - " +++ print e2
  print (Diff _ e1 e2) = print e1 +++ " - " +++ print e2
  print (DiffE _ e1 e2) = print e1 +++ " - " +++ print e2
  print (Mult _ e1 e2) = print e1 +++ " * " +++ print e2
  print (Inter _ e1 e2) = print e1 +++ " * " +++ print e2
  print (Scale _ e1 e2) = print e1 +++ " * " +++ print e2
  print (AttElem _ e1 e2) = print e1 +++ " = " +++ print e2
  print (AttSet _ e1 e2) = print e1 +++ " = " +++ print e2

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
  print (If p t e) = "if " +++ print p +++ "\nthen\n" +++ print t +++ "\n\telse\n" +++ print e
  print (For i set stmt) = "for " +++ i +++ " in " +++ print set +++ " do\n" +++ print stmt
  print (stmt1 :. stmt2) = print stmt1 +++ "\n" +++ print stmt2

// ----- Syntactic Sugar -----

class Lit a where
  lit :: a -> Expression a

class Var a where
  var :: Ident -> a

class =. a where
  (=.) infixl 2 :: Ident a -> Stmt

instance Lit Int where
  lit i = Elem bm i

instance Lit [Int] where
  lit s = New bm s

instance Var Elem where
  var i = VarElem bm i

instance Var Set where
  var i = VarSet bm i

instance =. Elem where
  (=.) i e = Expression $ AttElem bm i e

instance =. Set where
  (=.) i s = Expression $ AttSet bm i s

instance + Elem where
  (+) e1 e2 = Plus bm e1 e2

instance + Set where
  (+) s1 s2 = Union bm s1 s2

instance - Elem where
  (-) e1 e2 = Minus bm e1 e2

instance - Set where
  (-) s1 s2 = Diff bm s1 s2

instance * Elem where
  (*) e1 e2 = Mult bm e1 e2

instance * Set where
  (*) s1 s2 = Inter bm s1 s2

(+=) infixl 6 :: Set Elem -> Set
(+=) s e = AddSetEle bm s e

(=+) infixl 6 :: Elem Set -> Set
(=+) e s = AddEleSet bm e s

(-=) infixl 6 :: Set Elem -> Set
(-=) s e = DiffE bm s e

(=*) infixl 6 :: Set Elem -> Set
(=*) s e = Scale bm e s

// ----- Start -----

// The following functions solve the overloading problem when using "variable"
set :: (Set -> Set)
set = id

element :: (Elem -> Elem)
element = id

Start = eval $ "x" =. lit 4 :. "y" =. lit 5 :. "z" =. element (var "x")
  where
    eval e = id $ (unS (evalS e)) 'Map'.newMap

/*I tried to define a simulator on iTakss using the code below (with some extra
  qualified imports) and it did run, but the fields for the bimap (f, t) can't
  be defined (since it can't generate webforms for function types) and are simply
  blank (just the field names, no values/selectors) in the webpage. So no, we 
  can't define a simulator for this DSL using iTasks.

Start w = 'IT'.startEngine home w
  where
    home :: 'IT'.Task (Expression Int)
    home = 'IT'.enterInformation "Enter an expression" [] 

derive class iTask Expression, BM
derive class Publishable Expression*/












  

