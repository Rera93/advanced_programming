module serialize4

import StdEnv, Data.Maybe, monad

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming, week 4, 2017
	
	import StdMaybe from Libraries/StdLib
	use StdEnv or StdEnv 64
	use Basic Values Only as conclose option for a nicer output.
*/

// ---

:: State s a = S (s -> (Maybe a,s))

unS :: (State s a) -> s -> (Maybe a,s)
unS (S f) = f

instance MFunctor (State s) where
	fmap f (S x) = S \s -> case x s of
		(Just a,s) = (Just (f a),s)
		(_,s) = (Nothing,s)
instance MApplicative (State s) where
	pure a = S \s -> (Just a, s)
	(<*>) (S f) (S x) = S \s -> case f s of
		(Just f,s) = case x s of
			(Just x,s) = (Just (f x),s)
			(Nothing,s) = (Nothing,s)
		(Nothing,s) = (Nothing,s)
instance fail (State s) where
	fail = S \s.(Nothing,s)
instance MMonad (State s) where
	bind (S ma) f = S \s -> case ma s of
		(Just a,s) = unS (f a) s
		_ = (Nothing,s)
instance OrMonad (State s) where
	(<|>) f g = S \s -> case unS f s of
		(Just a, s) = (Just a, s)
		_ = unS g s

// ---

:: Serialized = { inp :: [String], out :: [String]}

ser :: Serialized
ser = {inp = [], out = []}

toStrings :: Serialized -> [String]
toStrings s = s.out

:: Serialize a :== State Serialized a

wrt :: a -> Serialize String | toString a
wrt a = S \s -> (Just (toString a), {s & out = [toString a:s.out]})

rd :: Serialize String
rd = S r where
	r s=:{inp=[a:as]} = (Just a, {s & inp = as})
	r s=:{inp=[],out=[]} = (Nothing, s)
	r s=:{inp=[],out=l} = r {s & inp=reverse l, out=[]}

match :: a -> Serialize a | toString a
match a = rd >>= \x -> guard (x == toString a) >>| (pure a)

pred :: (String->Bool) -> Serialize String
pred f = rd >>= \x -> guard (f x) >>| pure x

// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

:: Write a :== a -> Serialize String
:: Read a  :== Serialize a
 
class serialize a | isUNIT a where
  write :: a -> Serialize String
  read  :: Serialize a

class serialize1 t where
  write1 :: (Write a) (t a) -> Serialize String
  read1  :: (Read  a) -> Serialize (t a)

class serializeCONS a where
	writeCons :: (Write a) (CONS a) -> Serialize String
	readCons  :: String (Read a) -> Serialize (CONS a)

class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) -> Serialize String
  read2  :: (Read  a) (Read  b) -> Serialize (t a b)

class isUNIT a :: a -> Bool
instance isUNIT UNIT where isUNIT _ = True
instance isUNIT a    where isUNIT _ = False

instance serialize Bool where
  write b = wrt b
  read = match True <|> match False

instance serialize Int where
	write i = wrt i
	read = toInt <$> rd

instance serialize String where
	write s = wrt s
	read = rd

instance serialize UNIT where
	write _ = wrt ""
	read = pure UNIT

instance serializeCONS a where
	writeCons wa (CONS name a) = wrt "(" >>| wrt name >>| wa a >>| wrt ")"
	readCons name ra = match "(" >>| match name >>| ra >>= \a -> match ")" >>| pure (CONS name a)
 
instance serializeCONS UNIT where
	writeCons wa (CONS name a) = wrt name
	readCons name ra = match name >>| pure (CONS name UNIT)
 
instance serialize2 EITHER where
  write2 wa _ (LEFT  a) = wa a
  write2 _ wb (RIGHT b) = wb b
  read2 ra rb = LEFT <$> ra <|> RIGHT <$> rb

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) = wa a >>| wb b
  read2 ra rb = ra >>= \a -> rb >>= \b -> pure (PAIR a b)

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
 write a = write1 write a
 read    = read1  read

instance serialize1 [] where
	write1 _ [] = wrt NilString
	write1 wa [x:xs] = wrt ConsString >>| wa x >>| write1 wa xs
	read1 ra = pred (\x -> x == NilString) >>| pure []
		<|> pred (\x -> x == ConsString) >>| ra >>= \a -> read1 ra >>= \as -> pure [a:as]
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
	write b = write1 write b
	read = read1 read

instance serialize1 Bin where
	write1 _ Leaf = wrt LeafString
	write1 wa (Bin l a r) = wrt BinString >>| write1 wa l >>| wa a >>| write1 wa r
	read1 ra = pred (\x -> x == LeafString) >>| pure Leaf
		<|> pred (\x -> x == BinString) >>| read1 ra >>= \l -> ra >>= \a -> read1 ra >>= \r -> pure (Bin l a r)
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

instance toString Coin where
	toString Head = "Head"
	toString Tail = "Tail"

instance serialize Coin where
	write x = wrt (toString x)
	read = match Head <|> match Tail

// ---

instance serialize (a,b) | serialize a & serialize b where
	write (a,b) = write a >>| write b
	read = read >>= \a -> read >>= \b -> pure (a,b)

// ---

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test Head
  ,test Tail
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test [[[1]],[[2],[3,4]],[[]]]
  ,test [[True],[]]
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
test a = toStrings (snd ((unS t) ser)) where
 t
 	=   write a
	>>| read
	>>= \b. guard (a == b)
	>>| write "Oke "
	<|> write "Failure "

