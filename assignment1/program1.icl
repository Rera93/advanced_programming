// Matheus Amazonas Cabral de Andrade
// s4605640

module program1

import StdMaybe, StdString, StdChar, StdTuple, StdBool, StdList, StdOverloaded

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Rose a = Rose a [Rose a]

class serialize a where
    write :: a [String] -> [String]
    read :: [String] -> Maybe (a,[String])

instance serialize Bool where
    write b s = [toString b : s]
    read ["True":ss] = Just (True, ss)
    read ["False":ss] = Just (False, ss)
    read _ = Nothing

instance serialize Int where
    write i s = [toString i : s]
    read [n:ns] = Just (stringToInt (fromString n), ns)
    read _ = Nothing

// Read for bin, lists and rose is really, really a parser. 
// Is this necessary? Am I missing something?

instance serialize (Bin a) | serialize a where
    write Leaf s = ["Leaf" : s]
    write (Bin x n y) s = [w : s]
    where
        w = "(Bin " +++ show x +++ " " +++ show n +++ " " +++ show y +++ ")"
    read ["Leaf":ss] = Just (Leaf, ss)
    read _ = Nothing        // Missing this

instance serialize [a] | serialize a where
    write l s = ["[" +++ eles l +++ "]" : s]
    where
        eles [] = ""
        eles [x] = show x
        eles [x:xs] = show x +++ ", " +++ concat (map eles [xs])
    read ["[]":ss] = Just ([], ss)
    read _ = Nothing        // Missing this

instance serialize (Rose a) | serialize a where
    write (Rose x xs) s = [w : s]
    where
        w = "Rose " +++ show x +++ " " +++ show xs
    read _ = Nothing        // Missing this

concat :: [String] -> String
concat xs = foldl (\e x -> e +++ x) "" xs

show :: a -> String | serialize a
show x = hd (write x [])

test :: a -> (Bool,[String]) | serialize, ==a
test a = (isJust r && fst jr==a && isEmpty (tl (snd jr)), s)
where
    s = write a ["λ n"] 
    r = read s
    jr = fromJust r

stringToInt :: [Char] -> Int
stringToInt ['-':s] = 0 - (stringToInt s)
stringToInt s = foldl (\x y -> x*10 + digitToInt y) 0 s

// 2 - Kinds
//Bool, Bin, Rose, Bin Int, Tree, T1, T2, T3, and T4?
// Kind of Bool: *
// Kind of Bin: * -> *
// Kind of Rose: * -> *
// Kind of Bin Int: *
// Kind of Tree: * -> * -> *
// Kind of T1: (* -> *) -> * -> *
// Kind of T2: (* -> *) -> (* -> *) -> * -> *
// Kind of T3: (* -> * -> *) -> * -> * -> *
// Kind of T4: (* -> *) -> (* -> *) -> * -> *




Start = write [1] []

//Start = [test True, test False ]
//Start = [test 45, test -67 ]

//Start :: [String]
//Start = write (Bin (Bin Leaf 6 (Bin Leaf 9 Leaf)) 4 (Bin (Bin Leaf 5 Leaf) 7 Leaf)) []

//Start = write (Rose 1 []) []
//Start = stringToInt ['45']

//Start :: Int
//Start = read ["45", "-56"]



