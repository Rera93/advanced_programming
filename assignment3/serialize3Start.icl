// Matheus Amazonas Cabral de Andrade
// s4605640

module serialize3Start

/*
  Definitions for assignment 3 in AFP 2017
  Kind indexed gennerics
  Pieter Koopman, pieter@cs.ru.nl
  September 2017
  
  use environment: StdMaybe from Libraries/StdLib
*/

import StdEnv, StdMaybe

:: Write a :== a [String] -> [String]
:: Read a  :== [String] -> Maybe (a,[String])

class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])

// use this as serialize0 for kind *
class serialize0 a where
  write0 :: a [String] -> [String]
  read0  :: [String] -> Maybe (a,[String])

class serialize1 t where 
  write1 :: (Write a) (t a) [String] -> [String]
  read1 :: (Read a) [String] -> Maybe(t a, [String])

class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) [String] -> [String]
  read2 :: (Read a) (Read b) [String] -> Maybe (t a b, [String])

class serializeCONS a where 
  writeCons :: (Write a) (CONS a) [String] -> [String]
  readCons :: String (Read a) [String] -> Maybe (CONS a, [String])

// ---

instance serialize Bool where
  write b c = write0 b c
  read s = read0 s

instance serialize0 Bool where
  write0 b c = [toString b:c]
  read0 ["True":r] = Just (True,r)
  read0 ["False":r] = Just (False,r)
  read0 _ = Nothing

instance serialize Int where
  write i c = write0 i c
  read s = read0 s

instance serialize0 Int where
  write0 i c = [toString i:c]
  read0 [s:r]
    # i = toInt s
    | s == toString i
      = Just (i,r)
      = Nothing
  read0 _ = Nothing

// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

instance serialize UNIT where
  write u c = write u c
  read s = read0 s

instance serialize0 UNIT where
  write0 _ c = c
  read0 s = Just(UNIT, s)

instance serialize (PAIR a b) | serialize a & serialize b where
  write p s = write2 write write p s
  read s = read2 read read s

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) c = wa a (wb b c)
  read2 ra rb s = case ra s of
    Just (a, m) = case rb m of
      Just (b, n) = Just(PAIR a b, n)
      _ = Nothing
    _ = Nothing

instance serialize (EITHER a b) | serialize a & serialize b where
  write e s = write2 write write e s
  read s = read2 read read s

instance serialize2 EITHER where
  write2 wa _ (LEFT a) c = wa a c
  write2 _ wb (RIGHT b) c = wb b c
  read2 rl rr s = case rl s of
    Just (a, m) = Just (LEFT a, m)
    Nothing = case rr s of
      Just (b, n) = Just (RIGHT b, n)
      _ = Nothing

instance serialize (CONS a) | serialize a where
  write c s = write1 write c s
  read s = read1 read s

instance serialize1 CONS where
  write1 w (CONS cons a) c = ["(", cons : w a [")":c]]
  read1 r ["(", cons:s] = case r s of
    Just (a, [")":m]) = Just (CONS cons a, m)
    _ = Nothing
  read1 _ _ = Nothing

instance serializeCONS UNIT where
  writeCons _ (CONS c _) s = [c:s]
  readCons c ra [s:l] 
    | c == s = Just (CONS s UNIT, l)
  readCons _ _ _ = Nothing

instance serializeCONS a where
  writeCons _ a s = []
  readCons _ _ _ = Nothing

// ---

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList []  = LEFT  (CONS NilString  UNIT)
fromList [a:x] = RIGHT (CONS ConsString (PAIR a x))

toList :: (ListG a) -> [a]
toList (LEFT  (CONS NilString  UNIT)) = []
toList (RIGHT (CONS ConsString (PAIR a x))) = [a:x]

NilString :== "Nil"
ConsString :== "Cons"

instance serialize [a] | serialize a where
  write l s = write1 write l s
  read l = read1 read l

instance serialize1 [] where
  write1 w l s = write2 (writeCons write0) (write1 (write2 w (write1 w))) (fromList l) s
  read1 r s = case read2 (readCons "Nil" read0) (read1 (read2 r (read1 r))) s of
    Just (l, s) = Just (toList l, s)
    _ = Nothing

// ---

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS LeafString UNIT)
fromBin (Bin l a r) = RIGHT (CONS BinString (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR a r)))) = Bin l a r

LeafString :== "Leaf"
BinString :== "Bin"

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

instance serialize (Bin a) | serialize a where
  write a s = write1 write a s
  read s = read1 read s

instance serialize1 Bin where
  write1 w b s = write2 (writeCons write0) (write1 (write2 (write1 w) (write2 w (write1 w)))) (fromBin b) s
  read1 r s = case read2 (readCons "Leaf" read0) (read1 (read2 (read1 r) (read2 r (read1 r)))) s of
    Just (t, m) = Just (toBin t, m)
    _ = Nothing

// ---

:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS "Head" UNIT)
fromCoin Tail = RIGHT (CONS "Tail" UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS _ UNIT)) = Head
toCoin (RIGHT (CONS _ UNIT)) = Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

instance serialize Coin where
  write c s = write2 (writeCons write0) (writeCons write0) (fromCoin c) s
  read s = case read2 (readCons "Head" read) (readCons "Tail" read) s of
    Just (c, m) = Just (toCoin c, m)
    _ = Nothing

instance serialize0 Coin where
  write0 c s = write2 (write1 write0) (write1 write0) (fromCoin c) s
  read0 s = case read2 (read1 read0) (read1 read0) s of
    Just (c, s) = Just (toCoin c, s)
    _ = Nothing

/*
	Define a special purpose version for this type that writes and reads
	the value (7,True) as ["(","7",",","True",")"]
*/

:: TupleG a b :== PAIR (CONS a) (CONS b)

fromTuple :: (a,b) -> TupleG a b
fromTuple (a,b) = PAIR (CONS "L" a) (CONS "R" b)

toTuple :: (TupleG a b) -> (a, b)
toTuple (PAIR (CONS _ a) (CONS _ b)) = (a, b)

instance serialize (a, b) | serialize a & serialize b where
  write t s = write2 write write t s
  read s = read2 read read s

instance serialize2 (,) where
  write2 wa wb t c = write2 (write1 wa) (write1 wb) (fromTuple t) c
  read2 ra rb s = case ra s of
    Just (a, m) = case rb m of
      Just (b, n) = Just ((a, b), n)
      _ = Nothing
    _ = Nothing


// ---
// output looks nice if compiled with "Basic Values Only" for console in project options

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test [[[1]],[[2],[3,4]],[[]]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ,test Head
  ,test Tail
  ,test (7,True)
  ,test (Head,(7,[Tail]))
  ,["End of the tests.\n"]
  ]

test :: a -> [String] | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke"]
        ["Not all input is consumed! ":snd jr])
      ["Wrong result: ":write (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r


/* 1.1
I couldn't figure out how to make 1.1 work. I really, really tried with a new class
that determines when a given generic representation is a "leaf" representation, but 
I kept getting some "can't resolve overloading" errors
*/ 


/*  ------ OUTPUT ------
[["Oke",", write produces: ","True","
"],["Oke",", write produces: ","False","
"],["Oke",", write produces: ","0","
"],["Oke",", write produces: ","123","
"],["Oke",", write produces: ","-36","
"],["Oke",", write produces: ","(","Cons","42","Nil",")","
"],["Oke",", write produces: ","(","Cons","0","(","Cons","1","(","Cons","2","(","Cons","3","(","Cons","4","Nil",")",")",")",")",")","
"],["Oke",", write produces: ","(","Cons","(","Cons","True","Nil",")","(","Cons","Nil","Nil",")",")","
"],["Oke",", write produces: ","(","Cons","(","Cons","(","Cons","1","Nil",")","Nil",")","(","Cons","(","Cons","(","Cons","2","Nil",")","(","Cons","(","Cons","3","(","Cons","4","Nil",")",")","Nil",")",")","(","Cons","(","Cons","Nil","Nil",")","Nil",")",")",")","
"],["Oke",", write produces: ","(","Bin","Leaf","True","Leaf",")","
"],["Oke",", write produces: ","(","Cons","(","Bin","(","Bin","Leaf","(","Cons","1","Nil",")","Leaf",")","(","Cons","2","Nil",")","(","Bin","Leaf","(","Cons","3","Nil",")","(","Bin","Leaf","(","Cons","4","(","Cons","5","Nil",")",")","Leaf",")",")",")","Nil",")","
"],["Oke",", write produces: ","(","Cons","(","Bin","(","Bin","Leaf","(","Cons","1","Nil",")","Leaf",")","(","Cons","2","Nil",")","(","Bin","Leaf","(","Cons","3","Nil",")","(","Bin","(","Bin","Leaf","(","Cons","4","(","Cons","5","Nil",")",")","Leaf",")","(","Cons","6","(","Cons","7","Nil",")",")","(","Bin","Leaf","(","Cons","8","(","Cons","9","Nil",")",")","Leaf",")",")",")",")","Nil",")","
"],["Oke",", write produces: ","Head","
"],["Oke",", write produces: ","Tail","
"],["read result is Nothing",", write produces: ","(","L","7",")","(","R","True",")","
"],["read result is Nothing",", write produces: ","(","L","Head",")","(","R","(","L","7",")","(","R","(","Cons","Tail","Nil",")",")",")","
"],["End of the tests.
"]]
*/


