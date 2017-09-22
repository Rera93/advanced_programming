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
	Cshow l = []     // Where is the overloading that can't be solved? [toString l]
	Cnew = []

instance Container Bin where
	Cinsert x t = insertBin x t
	Ccontains x t = containsBin x t
	Cshow t = showBin t
	Cnew = Leaf

concat :: [String] -> String
concat xs = foldl (\e x -> e +++ x) "" xs

insertBin :: a (Bin a) -> Bin a
insertBin x Leaf = Bin Leaf x Leaf
insertBin x (Bin l e r) = Bin (insertBin x l) e r

containsBin :: a (Bin a) -> Bool | Eq a
containsBin _ Leaf = False
containsBin x (Bin l e r) 
	| e == x = True
	= containsBin x l || containsBin x r

showBin :: (Bin a) -> [String] | toString a
showBin Leaf = ["Leaf"]
showBin (Bin l e r) = ["(Bin " +++ concat (showBin l) 
					  +++ " " +++ toString e
					  +++ " " +++ concat (showBin r) +++ ")"]


//Start = Cinsert 5 (Bin (Bin Leaf 4 Leaf) 3 Leaf)
Start = Cshow (Bin (Bin Leaf 5 Leaf) 4 Leaf)