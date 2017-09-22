// Matheus Amazonas Cabral de Andrade
// s4605640

module program2

import StdLib, StdList, StdChar, StdString

:: Bin a = Leaf | Bin (Bin a) a (Bin a)


class Container t where
	Cinsert :: a (t a) -> t a | < a 
	Ccontains :: a (t a) -> Bool | <, Eq a 
	Cshow :: (t a) -> [String] | toString a 
	Cnew :: t a

instance Container [] where
	Cinsert x l = [x:l]
	Ccontains x l = isMember x l
	Cnew = []
	Cshow l = ["[": printEles l ["]"]]

printEles :: [a] [String] -> [String] | toString a
printEles [] c = c
printEles [x] c = [toString x : c]
printEles [x:xs] c = [toString x, ", " : printEles xs c]

instance Container Bin where
	Cinsert x Leaf = Bin Leaf x Leaf
	Cinsert x (Bin l e r)
		| x < e = Bin (Cinsert x l) e r
		        = Bin l e (Cinsert x r)
	Ccontains x Leaf = False
	Ccontains x (Bin l e r)
		| x < e = Ccontains x l
		| x > e = Ccontains x r
				= e == x
	Cnew = Leaf
	Cshow Leaf = ["Leaf"]
	Cshow t = ["(": printEles (treeToList t []) [")"]]
	where
		treeToList Leaf c = c
		treeToList (Bin l e r) c = treeToList l [e: treeToList r c]

//Start = Cshow (Bin (Bin Leaf 4 Leaf) 3 Leaf)
//Start = Cshow [1,2,3,4,5]