// Matheus Amazonas Cabral de Andrade
// s4605640

module serialize4

import StdEnv, StdMaybe, monad

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

instance MyFunctor (State s) where
  fmap f (S s) = S \t -> case s t of
    (Just a, s) = (Just (f a), s) 
    (_, s) = (Nothing, s)
instance Applicative (State s) where
  pure a = S \s -> (Just a, s)
  (<*>) (S f) (S x) = S \s -> case f s of
    (Just f, s) = case x s of
      (Just y, s) = (Just (f y), s)
      (_, s) = (Nothing, s)
    (_, s) = (Nothing, s)
instance fail (State s) where 
  fail = S \s -> (Nothing,s)
instance Monad (State s) where
  bind (S a) f = S \s -> case a s of
    (Just a, s) = unS (f a) s
    (_, s) = (Nothing, s)
instance OrMonad (State s) where
  (<|>) (S f) (S g) = S \s -> case f s of
    (Nothing, _) = g s
    other        = other

instance MyFunctor [] where
  fmap f l = map f l
instance Applicative [] where
  pure a = [a]
  (<*>) fs xs = [f x \\ f <- fs, x <- xs]
instance fail [] where
  fail = []
instance Monad [] where
  bind xs f = [y \\ x <- xs, y <- f x]
instance OrMonad [] where
  (<|>) [] y = y
  (<|>) x y = x


// ---

:: Serialized = Serialized [String] [String]

ser :: Serialized
ser = Serialized [] []

toStrings :: Serialized -> [String]
toStrings (Serialized _ os) = os

:: Serialize a :== State Serialized a

wrt :: a -> Serialize String | toString a
wrt a = S \(Serialized is os) -> (Just (toString a), Serialized is [toString a : os])
wrt a = S \(Serialized is os) -> (Just (toString a), (Serialized [toString a: is] (os++[toString a])))

rd :: Serialize String
rd = S \(Serialized is os) -> case is of
  [] -> case os of
    [] -> (Nothing, Serialized is os)
    l -> unS rd (Serialized (reverse l) [])
  [x:xs] -> (Just x, Serialized xs os)

match :: a -> Serialize a | toString a
match a = rd >>= \s -> guard(toString a == s) >>| pure a

pred :: (String->Bool) -> Serialize String
pred p = rd >>= \s -> guard (p s) >>| pure s

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

readInt :: String -> Maybe Int
readInt s
    # i = toInt s
    | s == toString i
      = Just i
      = Nothing

instance serialize Int where
  write i = wrt i
  read = rd
    >>= \s -> pure (readInt s)
    >>= \i -> case i of
      Just x -> pure x
      _ -> fail

instance serialize String where
	write s = wrt s
	read = rd

instance serialize UNIT where
	write _ = pure ""
	read = pure UNIT

instance serializeCONS UNIT where
  writeCons wa (CONS name a) = wrt name 
  readCons name _ = CONS <$> match name <*> pure UNIT
 
instance serializeCONS a where
  writeCons wa (CONS name a) = wrt "("
    >>| wrt name
    >>| wa a
    >>| wrt ")"
  readCons name ra = match "("
    >>| rd
    >>= \name -> ra 
    >>= \a -> match ")"
    >>| pure (CONS name a)

instance serialize2 EITHER where
  write2 wa wb (LEFT  a) = wa a
  write2 wa wb (RIGHT b) = wb b
  read2 ra rb = LEFT <$> ra <|> RIGHT <$> rb

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) = wa a >>| wb b
  read2 ra rb = PAIR <$> ra <*> rb

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
 read    = read1 read

instance serialize1 [] where
  write1 _ [] = wrt NilString
  write1 writea [a:as] = wrt ConsString >>| writea a >>| write1 writea as
  read1  reada = pred (\s -> s == NilString)
    >>| pure []
    <|> pred (\s -> s == ConsString)
    >>| reada 
    >>= \x -> read1 reada
    >>= \xs -> pure [x:xs]
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
  write1 wa Leaf = wrt LeafString
  write1 wa (Bin l a r) = wrt BinString >>| write1 wa l >>| wa a >>| write1 wa r
  read1 ra = pred (\s -> s == LeafString) 
    >>| pure Leaf
    <|> pred (\s -> s == BinString) 
    >>| read1 ra 
    >>= \l -> ra 
    >>= \a -> read1 ra
    >>= \r -> pure (Bin l a r)
  //read1  reada = match Leaf 
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

HeadString :== "Head"
TailString :== "Tail"

instance serialize Coin where
  write Head = wrt HeadString
  write Tail = wrt TailString
  read = match Head <|> match Tail

instance toString Coin where
  toString Head = HeadString
  toString Tail = TailString

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

//test :: a -> [String] | serialize, == a
test a = toStrings (snd ((unS t) ser)) where
 t
 	=   write a
	>>| read
	>>= \b. guard (a == b)
	>>| write "Oke"
	<|> write "Failure"

