module serialize3

import StdEnv, Data.Maybe
from Data.Func import $

:: Write a :== a [String] -> [String]
:: Read a  :== [String] -> Maybe (a,[String])

// use this as serialize0 for kind *
class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])

class serialize0 a where
  write0 :: a [String] -> [String]
  read0 :: [String] -> Maybe (a, [String])

class serialize1 t where
  write1 :: (Write a) (t a) [String] -> [String]
  read1 :: (Read a) [String] -> Maybe (t a, [String])

class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) [String] -> [String]
  read2 :: (Read a) (Read b) [String] -> Maybe (t a b, [String])

class serializeCons a where
  writeCons :: (Write a) (CONS a) [String] -> [String]
  readCons :: String (Read a) [String] -> Maybe (CONS a, [String])

// ---

instance serialize2 EITHER where
  write2 wa _ (LEFT a) s = wa a s
  write2 _ wb (RIGHT b) s = wb b s
  read2 ra rb s = case ra s of
    Just (a,s) = Just (LEFT a, s)
    _ = case rb s of
      Just (b,s) = Just (RIGHT b, s)
      _ = Nothing

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) s = wa a (wb b s)
  read2 ra rb s = case ra s of
    Just (a, s) = case rb s of 
      Just (b, s) = Just (PAIR a b, s)
      _ = Nothing
    _ = Nothing

instance serialize1 CONS where
  write1 wa (CONS c a) s = ["(", c:wa a [")":s]]
  read1 ra ["(",c:s] = case ra s of
    Just (a,[")":s]) = Just (CONS c a, s)
    _ = Nothing
  read1 _ _ = Nothing

instance serializeCons a where
  writeCons wa (CONS c a) s = ["(", c:wa a [")":s]]
  readCons x ra ["(",c:s]
    | x == c = case ra s of
      Just (a, [")":s]) = Just (CONS x a, s)
      _ = Nothing
  readCons _ _ _ = Nothing

instance serialize0 UNIT where
  write0 _ s = s
  read0 s = Just (UNIT, s)

instance serialize0 Bool where
  write0 b s = write b s
  read0 s = read s

instance serialize0 Int where
  write0 i s = write i s
  read0 s = read s

instance serialize Bool where
  write b c = [toString b:c]
  read ["True":r] = Just (True,r)
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

// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

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
 write a s = write1 write a s
 read  s   = read1 read s

instance serialize1 [] where
  write1 wa a s = write2 (write1 write0) (write1 (write2 wa (write1 wa))) (fromList a) s
  read1 ra s = case read2 (read1 read0) (read1 (read2 ra (read1 ra))) s of
    Just (a,s) = Just (toList a, s)
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
	write b s = write1 write b s
	read    l = read1 read l

instance serialize1 Bin where
  write1 wa a s = write2 (write1 write0) (write1 (write2 (write1 wa) (write2 wa (write1 wa)))) (fromBin a) s
  read1 ra s = case read2 (readCons "Leaf" read0) (read1 (read2 (read1 ra) (read2 ra (read1 ra)))) s of
    Just (a,s) = Just (toBin a, s)
    _ = Nothing

// ---

:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS "Head" UNIT)
fromCoin Tail = RIGHT (CONS "Tail" UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS "Head" UNIT)) = Head
toCoin (LEFT (CONS c UNIT)) = abort ("Head with cons: " +++ c)
toCoin (RIGHT (CONS "Tail" UNIT)) = Tail
toCoin (RIGHT (CONS c UNIT)) = abort ("Tailw ith cons: " +++ c)

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

instance serialize Coin where
	write c s = write0 c s
	read    l = read0 l

instance serialize0 Coin where
  write0 c s = write2 (write1 write0) (write1 write0) (fromCoin c) s
  read0 s = case read2 (readCons "Head" read0) (readCons "Tail" read0) s of
    Just (c,s) = Just (toCoin c, s)
    _ = Nothing

/*
	Define a special purpose version for this type that writes and reads
	the value (7,True) as ["(","7",",","True",")"]
*/

:: TupleG a b :== PAIR a b

fromTuple :: (a,b) -> PAIR a b
fromTuple (a,b) = PAIR a b

toTuple :: (PAIR a b) -> (a,b)
toTuple (PAIR a b) = (a,b)

instance serialize (a,b) | serialize a & serialize b where
	write (a,b) c = write2 write write (a,b) c
	read s = read2 read read s 

instance serialize2 (,) where
  write2 wa wb (a,b) s = ["(":wa a [",":wb b [")":s]]]
  read2 ra rb ["(":s] = case ra s of
    Just (a,[",":s]) = case rb s of
      Just (b,[")":s]) = Just ((a,b), s)
      _ = Nothing
    _ = Nothing
  read2 _ _ _ = Nothing

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
