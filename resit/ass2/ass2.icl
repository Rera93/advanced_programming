module ass2

/*
  Definition for assignment 2 in AFP 2017
  Pieter Koopman pieter@cs.ru.nl
  September 2017
*/

import StdEnv, Data.Maybe

class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])

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
  write _ s = s
  read s = Just (UNIT, s)

instance serialize (PAIR a b) | serialize a & serialize b where
  write (PAIR a b) s = write a (write b s)
  read s = case read s of
    Just (a, s) = case read s of
      Just (b, s) = Just (PAIR a b, s)
      _ = Nothing
    _ = Nothing

instance serialize (EITHER a b) | serialize a & serialize b where
  write (LEFT a) s = write a s
  write (RIGHT b) s = write b s
  read s = case read s of
    Just (a,s) = Just (LEFT a, s)
    _ = case read s of
      Just (b,s) = Just (RIGHT b, s)
      _ = Nothing

instance serialize (CONS a) | serialize a where
  write (CONS c a) s = ["(",c:write a [")":s]]
  read ["(",c:s] = case read s of
    Just (a, [")":s]) = Just (CONS c a, s)
    _ = Nothing
  read _ = Nothing

// ---

:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

// ---

:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

instance serialize [a] | serialize a where
  write l s = write (fromList l) s
  read s = case read s of
    Just (l,s) = Just (toList l, s)
    _ = Nothing

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

instance serialize (Bin a) | serialize a where // to be imporved
  write a s = write (fromBin a) s
  read l = case read l of
    Just (b,s) = Just (toBin b, s)
    _ = Nothing
  read _ = Nothing

instance == (Bin a) | == a where // better use the generic approach
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

fromList :: [a] -> ListG a
fromList [] = LEFT (CONS "[]" UNIT)
fromList [x:xs] = RIGHT (CONS "Cons" (PAIR x xs))

toList :: (ListG a) -> [a] 
toList (LEFT (CONS _ UNIT)) = []
toList (RIGHT (CONS _ (PAIR x xs))) = [x:xs]

fromBin :: (Bin a) -> BinG a 
fromBin Leaf = LEFT (CONS "Leaf" UNIT)
fromBin (Bin l e r) = RIGHT (CONS "Bin" (PAIR l (PAIR e r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR e r)))) = Bin l e r

// ---

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
