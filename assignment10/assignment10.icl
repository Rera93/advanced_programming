module assignment10

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Either
import StdString
import Data.Maybe
import StdList 
import StdFunc
import StdBool
import Data.List
from Data.Func import $

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

// ----- State -----

:: State :== 'Map'.Map String (Either Int [Int])
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

storeE :: Ident Int (BM a Int) -> Sem a
storeE i v bm = S $ \s -> (bm.f v, 'Map'.put i (Left v) s)

readE :: Ident -> Sem Int
readE i = S $ \s -> case 'Map'.get i s of
  Just (Left v) -> (v, s)
  _ -> (0,s)

storeS :: Ident [Int] (BM a [Int]) -> Sem a
storeS i v bm = S $ \s -> (bm.f v, 'Map'.put i (Right v) s)

readS :: Ident -> Sem [Int]
readS i = S $ \s -> case 'Map'.get i s of
  Just (Right v) -> (v, s)
  _ -> ([],s)

Start = 1

// ----- evalEuation -----

evalE :: (Expression a) -> Sem a
evalE (New bm set) = pure $ bm.f set
evalE (Elem bm ele) = pure $ bm.f ele
evalE (VarElem bm var) = readE var >>= \v -> pure $ bm.f v
evalE (VarSet bm var) = readS var >>= \v -> pure $ bm.f v
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
evalE (AttElem bm i ee) = evalE ee >>= \e -> storeE i e bm
evalE (AttSet bm i se) = evalE se >>= \s -> storeS i s bm

evalL :: Logical -> Sem Bool
evalL TRUE = pure True
evalL FALSE = pure False
evalL (ee In se) = evalE ee >>= \e -> evalE se >>= \s -> pure $ isMember e s
evalL (e1 ==. e2) = evalE e1 >>= \v1 -> evalE e2 >>= \v2 -> pure $ v1 == v2
evalL (e1 <=. e2) = evalE e1 >>= \v1 -> evalE e2 >>= \v2 -> pure $ v1<=v2
evalL (Not e) = evalL e >>= \l -> pure $ not l
evalL (e1 ||. e2) = evalL e1 >>= \b1 -> evalL e2 >>= \b2 -> pure $ b1 || b2
evalL (e1 &&. e2) = evalL e1 >>= \b1 -> evalL e2 >>= \b2 -> pure $ b1 && b2



















  

