module program2 

import StdGeneric
import StdString
import StdTuple
import StdBool
import StdEnum
import Data.Maybe
import Data.List

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Coin = Head | Tail

instance == Coin where
	(==) Head Head = True
	(==) Tail Tail = True
	(==) _    _    = False

instance == (Bin a) | == a where
	(==) Leaf Leaf = True
	(==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
	(==) _ _ = False

generic write a :: a [String] -> [String] 
generic read a :: [String] -> Maybe (a,[String])

class serialize a | write{|*|}, read{|*|} a

write{|UNIT|} _ s = s
read{|UNIT|} s = Just (UNIT,s) 

write{|CONS of {gcd_name}|} wa (CONS a) s = ["(",gcd_name:wa a [")":s]]
read{|CONS of {gcd_name}|} ra ["(",c:s] 
	| c == gcd_name = case ra s of
		Just (a,[")":s]) = Just (CONS a,s)
		_ = Nothing
	= Nothing
read{|CONS|} _ _ = Nothing

write{|OBJECT|} wa (OBJECT a) s = wa a s
read{|OBJECT|} ra s = case ra s of
	Just (a,s) = Just (OBJECT a,s)
	_ = Nothing

write{|PAIR|} wa wb (PAIR a b) s = wa a (wb b s)
read{|PAIR|} ra rb s = case ra s of
	Just (a,s) = case rb s of
		Just (b,s) = Just (PAIR a b, s)
		_ = Nothing
	_ = Nothing

write{|EITHER|} wa _ (LEFT a) s = wa a s
write{|EITHER|} _ wb (RIGHT b) s = wb b s
read{|EITHER|} ra rb s = case ra s of
	Just (a,s) = Just (LEFT a, s)
	Nothing = case rb s of
		Just (b,s) = Just (RIGHT b, s)
		_ = Nothing

write{|Int|} i s = [toString i:s]
read{|Int|} [s:r] = Just (toInt s,r)

write{|Bool|} b s = [toString b:s]
read{|Bool|} ["True":r] = Just (True,r)
read{|Bool|} ["False":r] = Just (False,r)

derive class serialize [], Coin, (,), Bin

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
      ["Wrong result: ":write{|*|} (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write{|*|} a ["\n"]
    r = read{|*|} s
    jr = fromJust r