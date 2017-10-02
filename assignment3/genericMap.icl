// Matheus Amazonas Cabral de Andrade
// s4605640

module genericMap

import StdEnv, StdGeneric, GenEq

generic gMap a b :: a -> b
gMap{|Int|}         x = x
gMap{|Real|}        x = x
gMap{|UNIT|}        x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)

derive gMap [], Bin, (,)

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)
l = [1..7]

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)


// -------------- QUESTION 3 --------------

// gMap
//Start = gMap{|*->*|} fac t
//Start = gMap{|*->*|} (\x -> (x, fac x)) l
//Start = gMap{|*->*->*|} (gMap{|*->*|} fac) (gMap{|*->*|} fac) (l, t)

// gEq
//Start = gEq{|*|} [1,2] [3,4]
//Start = gEq{|*|} [1,2] [2,3]
Start = gEq{|*->*|} (<) [1,2] [2,3]
	
