// Matheus Amazonas Cabral de Andrade
// s4605640

module program2

import StdGeneric, StdMaybe, StdEnv

:: Coin = Head | Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False

generic write a :: a [String] -> [String]
generic read a :: [String] -> Maybe (a, [String])

class serialize a | write{|*|} a & read{|*|} a

write {|UNIT|} _ s = s
read {|UNIT|} s = Just(UNIT, s)

write {|PAIR|} wa wb (PAIR a b) s = wa a (wb b s)
read {|PAIR|} ra rb s = case ra s of
	Just(a, m) = case rb m of
		Just(b, n) = Just(PAIR a b, n)
		_ = Nothing
	_ = Nothing

write {|EITHER|} wa _ (LEFT a) s = wa a s
write {|EITHER|} _ wb (RIGHT b) s = wb b s
read {|EITHER|} ra rb s = case ra s of
	Just (a, m) = Just(LEFT a, m)
	Nothing = case rb s of
		Just (b, n) = Just (RIGHT b, n)
		_ = Nothing

write {|CONS|} wa (CONS a) s = wa a s
read {|CONS|} ra s = case ra s of
	Just (a, m) = Just(CONS a, m)
	_ = Nothing

write {|OBJECT|} wa (OBJECT a) s = wa a s
read {|OBJECT|} ra s = case ra s of
	Just (a, m) = Just (OBJECT a, m)

write {|FIELD|} wa (FIELD f) s = wa f s
read {|FIELD|} ra s = case ra s of
	Just (a, m) = Just(FIELD a, m)

write {|Int|} i s = [toString i:s]
read {|Int|} [s:r]
    # i = toInt s
    | s == toString i
      = Just (i,r)
      = Nothing
read {|Int|} _ = Nothing

write {|Bool|} b c = [toString b:c]
read {|Bool|} ["True":r] = Just (True,r)
read {|Bool|} ["False":r] = Just (False,r)
read {|Bool|} _ = Nothing

derive write [], Bin, Coin, (,)
derive read [], Bin, Coin, (,)


/* ---------------- README ----------------
For some weird reason, running this module throws a segmentation
fault in my machine. I'm not sure if it's because I downloaded a
nightly build that might be faulty, or if I did something wrong.
I don't see how in Clean I might have done something so wrong to
throw a segmentation fault
*/

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




