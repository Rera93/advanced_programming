module ass1 

import Data.Maybe
import StdString
import StdBool
import StdTuple
import Data.List
import StdMisc
from Data.Func import $

:: Bin a = Leaf | Bin (Bin a) a (Bin a) 
:: Rose a = Rose a [Rose a]

class serialize a where
	write :: a [String] -> [String]
	read :: [String] -> Maybe (a, [String])

instance serialize Bool where
	write b s = [toString b:s]
	read ["True":s] = Just (True, s)
	read ["False":s] = Just (False, s)
	read _ = Nothing

instance serialize Int where
	write i s = [toString i: s]
	read [s:r] = Just (toInt s, r)

instance serialize [a] | serialize a where
	write [] s = ["[]":s]
	write [x:xs] s = write "Cons(" (write x (write xs [")":s]))
	read ["[]":s] = Just ([], s)
	read ["Cons(":s] = case read s of
		Just (x,s) = case read s of
			Just (y,[")":s]) = Just ([x:y], s)
			_ = Nothing
		_ = Nothing
	read _ = Nothing

instance serialize String where
	write s r = [s:r]
	read [s:r] = Just (s, r)

instance serialize (Bin a) | serialize a where
	write Leaf s = ["Leaf":s]
	write (Bin l e r) s =  write "Bin(" (write l (write e (write r [")":s])))
	read ["Leaf":s] = Just (Leaf, s)
	read ["Bin(":s] = case read s of
		Just (l,s) = case read s of
			Just (e,s) = case read s of
				Just (r,[")":s]) = Just (Bin l e r, s)
				_ = Nothing
			_ = Nothing
		_ = Nothing
	read _ = Nothing

instance serialize (Rose a) | serialize a where
	write (Rose a r) s = write "Rose(" (write a (write r [")":s]))
	read ["Rose(":s] = case read s of
		Just (a,s) = case read s of
			Just (r,[")":s]) = Just (Rose a r, s)
			_ = Nothing
		_ = Nothing
	read _ = Nothing

instance == (Bin a) | == a where
	(==) Leaf Leaf = True
	(==) (Bin l1 e1 r1) (Bin l2 e2 r2) = l1 == l2 && e1 == e2 && r1 == r2
	(==) _ _ = False

instance == (Rose a) | == a where
	(==) (Rose a1 r1) (Rose a2 r2) = a1 == a2 && r1 == r2

test :: a -> (Bool, [String]) | serialize, == a
test x = (isJust r && fst (fromJust r) == x && isEmpty (snd (fromJust r)), s)
	where
		s = write x []
		r = read s

Start :: Maybe  (Rose Int, [String])
Start = read $ write r []
	where
		b :: Bin Int
		b = Bin (Bin Leaf 1 Leaf) 2 (Bin Leaf 3 Leaf)
		r :: Rose Int
		r = Rose 2 [Rose 3 [Rose 4 []], Rose 5 []]
		l :: [Int]
		l = [1,2,3]

// Bool: *
// Bin: * -> *
// Rose: * -> *
// Bin Int: *
// Tree: * -> * -> *
// T1: (* -> *) -> * -> *
// T2: (* -> *) -> (* -> *) -> * -> *
// T3: (* -> * -> *) -> * -> * -> *
// T4: (* -> *) -> (* -> *) -> * -> *

class Container t where
	Cinsert :: a (t a) -> t a | < a
	Ccontain :: a (t a) -> Bool | <, == a
	Cshow :: (t a) -> [String] | toString a
	Cnew :: t a

instance Container [] where
	Cinsert a l = [a:l]
	Ccontain a l = elem a l
	Cshow [] = ["[]"]
	Cshow l = ["[":[toString x +++ "," \\ x <- l]] ++ ["]"]
	Cnew = []

instance Container Bin where
	Cinsert a Leaf = Bin Leaf a Leaf
	Cinsert a (Bin l e r)
		| a<e = Bin (Cinsert a l) e r
		= Bin l e (Cinsert a r)
	Ccontain a Leaf = False
	Ccontain a (Bin l e r)
		| a<e = Ccontain a l
		| a>e = Ccontain a r
		= True
	Cshow _ = undef
	Cnew = Leaf







