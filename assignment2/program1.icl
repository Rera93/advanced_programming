module program1

import StdMaybe, StdString, StdChar, StdEnv

:: UNIT = UNIT
:: PAIR a b = PAIR a b 
:: EITHER a b = LEFT a | RIGHT b
:: CONS a = CONS String a

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

class serialize a where
    write :: a [String] -> [String]
    read :: [String] -> Maybe (a,[String])

instance serialize Int where
    write i s = [toString i : s]
    read [n:ns] = Just (stringToInt (fromString n), ns)
    read _ = Nothing

instance serialize UNIT where
	write _ c = ["UNIT":c]
	read ["UNIT":s] = Just (UNIT, s)
	read _ = Nothing

instance serialize (PAIR a b) | serialize a & serialize b where
	write (PAIR a b) c = ["(", "PAIR" : write a (write b [")":c])]
	read ["(", "PAIR" : s] = case read s of
		Just (a, s1) = case read s1 of
			Just (b, [")":s2]) = Just (PAIR a b, s2)
			_ = Nothing
		_ = Nothing
	read _ = Nothing

instance serialize (EITHER a b) | serialize a & serialize b where
	write (LEFT a) c = ["(", "LEFT" : write a [")":c]]
	write (RIGHT b) c = ["(", "RIGHT" : write b [")":c]]
	read ["(", "LEFT":s] = case read s of
		Just (a, [")":c]) = Just (LEFT a, c)
		_ = Nothing
	read ["(", "RIGHT":s] = case read s of
		Just (b, [")":c]) = Just (RIGHT b, c)
		_ = Nothing
	read _ = Nothing

instance serialize (CONS a) | serialize a where
	write (CONS s a) c = ["(", "CONS", s : write a [")":c]]
	read ["(", "CONS", c : s] = case read s of
		Just (a, [")":r]) = Just (CONS c a, r)
		_ = Nothing
	read _ = Nothing

instance serialize [a] | serialize a where
	write l c = write (fromList l) c
	read x = case read x of
		Just (l, s) = Just (toList l, s)
		_ = Nothing

instance serialize (Bin a) | serialize a where
	write b c = write (fromBin b) c
	read x = case read x of
		Just (b, s) = Just (toBin b, s)
		_ = Nothing

stringToInt :: [Char] -> Int
stringToInt ['-':s] = 0 - (stringToInt s)
stringToInt s = foldl (\x y -> x*10 + digitToInt y) 0 s

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "[]" UNIT)
fromList [x:xs] = RIGHT(CONS "Cons" (PAIR x xs))

toList :: (ListG a) -> [a]
toList (LEFT (CONS _ UNIT)) = []
toList (RIGHT(CONS _ (PAIR x xs))) = [x:xs]

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS "Leaf" UNIT)
fromBin (Bin l e r) = RIGHT (CONS "Bin" (PAIR l (PAIR e r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR e r)))) = Bin l e r

//Start :: Maybe ([Int], [String])
Start =  (write [1,2,3,7,8,9] [])
//Start:: Maybe (Bin Int, [String])
//Start = read (write (Bin (Bin  (Bin Leaf 3 Leaf) 1 (Bin Leaf 4 Leaf)) 6 (Bin Leaf 8 Leaf)) [])

/*  Answers to question 1

1.1: Not really, since the type UNIT has only 1 constructor (UNIT).
If there's only one constructor and the type isn't parametric, comparing
two elements of that type will always return True. In fact, we could define 
the instance ignoring the values:

instance == UNIT where
	(==) _ _ = True

1.2: The CONST type is there to add constructor name, which comes handy when 
we want to print the data, but it doesn't affect the actual values, hence
we shouldn't compare the names.

1.3: 
Leaf != [] (in their generic representation) because they have different
daya types (the expression doesn't even type checks). The generic reprresentation
of Leaf has a type of BinG and the generic representation of [] has a type of
ListG. Besides that, their constructor name (string) doesn't match. In addition,
 == isn't defined for those data types.

*/