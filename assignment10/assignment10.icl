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

storeE :: Ident Int -> Sem Int
storeE i v = S $ \s -> (v, 'Map'.put i (Left v) s)

readE :: Ident -> Sem Int
readE i = S $ \s -> case 'Map'.get i s of
  Just (Left v) -> (v, s)
  _ -> (0,s)

storeS :: Ident [Int] -> Sem [Int]
storeS i v = S $ \s -> (v, 'Map'.put i (Right v) s)

readS :: Ident -> Sem [Int]
readS i = S $ \s -> case 'Map'.get i s of
  Just (Right v) -> (v, s)
  _ -> ([],s)

Start = 1
