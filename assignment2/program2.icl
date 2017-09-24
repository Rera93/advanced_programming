// Matheus Amazonas Cabral de Andrade
// s4605640

module program2

import StdMaybe,  StdEnv

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

instance == (Bin a) | == a where // better use the generic approach
	(==) Leaf Leaf = True
	(==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
	(==) _ _ = False

instance serialize Bool where
	write b c = [toString b:c]
	read ["True":r]  = Just (True,r)
	read ["False":r] = Just (False,r)
	read _ = Nothing

instance serialize Int where
  write i c = [toString i:c]
  read [s:r]
    # i = toInt s
    | s == toString i
      = Just (i,r)
      = Nothing
  read _ = Nothing

instance serialize UNIT where
	write _ c = c
	read s = Just (UNIT, s)
	read _ = Nothing

instance serialize (PAIR a b) | serialize a & serialize b where
	write (PAIR a b) c = ["(" : write a (write b [")":c])]
	read ["(":s] = case read s of
		Just (a, s1) = case read s1 of
			Just (b, [")":s2]) = Just (PAIR a b, s2)
			_ = Nothing
		_ = Nothing
	read _ = Nothing

instance serialize (EITHER a b) | serialize a & serialize b where
	write (LEFT a) c = write a c
	write (RIGHT a) c = write a c
	read list = case read list of
		Just (a, m) = Just (LEFT a, m)
		Nothing = case read list of
			Just (b, m) = Just (RIGHT b, m)
			_ = Nothing
	read _ = Nothing

instance serialize (CONS a) | serialize a where
	write (CONS cons a) c = ["(", cons : write a [")":c]]
	read ["(", cons:s] = case read s of
		Just (a, [")":r]) = Just (CONS cons a, r)
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

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ]

test :: a -> ([String],[String]) | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke "]
        ["Fail: not all input is consumed! ":snd jr])
      ["Fail: Wrong result ":write (fst jr) []])
    ["Fail: read result is Nothing "]
  , ["write produces ": s]
  )
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r


/*  Answers to question 3

I couldn't come up with an example that breaks the system. First, I tried to define
a cyclic data type, functions to convert it, an instance of serialize and one of ==. 
But when I ran the tests, it simply overflows the heap, which isn't quite "breaking
the system", it's just a memory limitation. Testing with infinite data structures 
gave the same outcome.

I tried to play with many examples with different combinations of data types, but I
couldn't break the system. Of course, there is one data type we can't represent using
EITHER, PAIR, UNIT and CONS: functions (basically anything that doesn't have a kind of *).
But again, I don't think that counts as "breaking the system".

*/