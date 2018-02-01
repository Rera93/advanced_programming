module question5

import question2, question4
import StdList
from Data.Func import $

merge :: [a] [a] -> [a]
merge [] b = b
merge [a:as] bs = [a:merge bs as]

instance gram [TREE] where
	lit s = [LIT s]
	idn = [IDN "a", IDN "b"]
	int = [INT 0, INT 1]
	sequ [] = [SEQ []]
	sequ [t:ts] = [SEQ [x:xs] \\ (x,SEQ xs) <- diag2 t (sequ ts)]	// From the solution
	alt [] = []
	alt [x:xs] = merge x (alt xs)
	def f = let (p,q) = f p in q 	//  Obligatory WTH

:: Strings :== [[String]]
instance gram Strings where		// Why can't we define for [String]? Imagine this in the exam.
	lit s = [[s]]
	idn = [["a"], ["b"]]
	int = [["1"], ["2"]]
	sequ [] = [[]]
	sequ [s:ss] = [ x ++ xs \\ (x,xs) <- diag2 s (sequ ss)]	// From the solution
	alt [] = [[]]
	alt [s:ss] = merge s (alt ss)
	def f = let (p,q) = f p in q 	//  Obligatory WTH

Start :: Strings
Start = def \list -> (alt [lit "[]", sequ [lit "[", int, lit ":", list, lit "]"]],
							 sequ [idn, lit "=", list])