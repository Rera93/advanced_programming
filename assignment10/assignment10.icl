module assignment10

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Either
import StdString
import Data.Maybe
import StdList 
import StdFunc
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
  | Plus 	  (BM a Int)   Elem Elem
  | Union     (BM a [Int]) Set Set
  | AddEleSet (BM a [Int]) Elem Set
  | AddSetEle (BM a [Int]) Set Elem
  | Minus     (BM a Int)   Elem Elem
  | Diff	  (BM a [Int]) Set Set 
  | DiffE     (BM a [Int]) Set Elem
  | Mult	  (BM a Int)   Elem Elem
  | Inter 	  (BM a [Int]) Set Set
  | Scale	  (BM a [Int]) Elem Set
  | AttElem   (BM a Int)   Ident Elem
  | AttSet    (BM a [Int]) Ident Set

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

// ----- Evaluation -----

eval :: (Expression a) -> Sem a
eval (New bm set) = pure $ bm.f set
eval (Elem bm ele) = pure $ bm.f ele
eval (VarElem bm var) = readE var >>= \v -> pure $ bm.f v
eval (VarSet bm var) = readS var >>= \v -> pure $ bm.f v
eval (Size bm eSet) = eval eSet >>= \set -> pure $ bm.f $ length set
eval (Plus bm e1 e2) = eval e1 >>= \x1 -> eval e2 >>= \x2 -> pure $ bm.f $ x1+x2
eval (Union bm e1 e2) = eval e1 >>= \s1 -> eval e2 >>= \s2 -> pure $ bm.f $ union s1 s2
eval (AddEleSet bm ee se) = eval ee >>= \e -> eval se >>= \s -> pure $ bm.f $ union s [e]
eval (AddSetEle bm se ee) = eval ee >>= \e -> eval se >>= \s -> pure $ bm.f $ union s [e]
eval (Minus bm e1 e2) = eval e1 >>= \x1 -> eval e2 >>= \x2 -> pure $ bm.f $ x1-x2
eval (Diff bm e1 e2) = eval e1 >>= \s1 -> eval e2 >>= \s2 -> pure $ bm.f $ difference s1 s2
eval (DiffE bm e1 e2) = eval e1 >>= \s -> eval e2 >>= \e -> pure $ bm.f $ difference s [e]
eval (Mult bm e1 e2) = eval e1 >>= \x1 -> eval e2 >>= \x2 -> pure $ bm.f $ x1*x2
eval (Inter bm e1 e2) = eval e1 >>= \s1 -> eval e2 >>= \s2 -> pure $ bm.f $ intersect s1 s2
eval (Scale bm ee se) = eval ee >>= \e -> eval se >>= \s -> pure $ bm.f $ map ((*)e) s
eval (AttElem bm i ee) = eval ee >>= \e -> storeE i e bm
eval (AttSet bm i se) = eval se >>= \s -> storeS i s bm

