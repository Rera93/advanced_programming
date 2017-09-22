// Matheus Amazonas Cabral de Andrade
// s4605640

module program1

import StdMaybe, StdString, StdChar, StdTuple, StdBool, StdList, StdOverloaded, StdInt

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

instance serialize [a] | serialize a where
    write [] s = ["[]" : s]
    write [x:xs] s = ["(", "Cons" : write x (write xs [")":s])]
    read ["[]":ss] = Just ([], ss)
    read ["(", "Cons":x] = 
        case read x of
            Just (e, rs) = case read rs of
                Just (p, [")":t]) = Just ([e:p], t)
                _ = Nothing
            _ = Nothing
    read _ = Nothing

instance serialize (Bin a) | serialize a where
    write Leaf s = ["Leaf" : s]
    write (Bin x n y) s = ["(", "Bin" : write x (write n (write y [")":s]))]
    read ["Leaf":ss] = Just (Leaf, ss)
    read ["(", "Cons": x] = 
        case read x of
            Just (l, s1) = case read s1 of
                Just (e, s2) = case read s2 of
                    Just (r, [")":s3]) = Just (Bin l e r, s3)
                    _ = Nothing
                _ = Nothing
            _ = Nothing
    read _ = Nothing

instance serialize (Rose a) | serialize a where
    write (Rose x xs) s = ["(","Rose": write x (write xs [")":s])]
    read ["(","Rose":x] = 
        case read x of
            Just (a, s1) = case read s1 of
                Just (as, s2) = Just (Rose a as, s2)
                _ = Nothing
            _ = Nothing
    read _ = Nothing        

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

Start = case read (write l []) of
    Just (k,[]) = k == l
    _ = False
where
    l = [1,2,3,4,5,6,7]


