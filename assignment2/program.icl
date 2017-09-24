module program

:: UNIT = UNIT
:: PAIR a b = PAIR a b 
:: EITHER a b = LEFT a | RIGHT b
:: CONS a = CONS String a

:: List a = Nil | Cons a (List a)
:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a (List a)))
:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromList :: (List a) -> ListG a
fromList Nil = LEFT (CONS "Nil" UNIT)
fromList (Cons a as) = RIGHT(CONS "Cons" (PAIR a as))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS "Leaf" UNIT)
fromBin (Bin l e r) = RIGHT (CONS "Bin" (PAIR l (PAIR e r)))



Start = (fromList Nil)

/*  Answers to question 1

1.1: Not really, since the type UNIT has only 1 constructor (UNIT).
If there's only one constructor and the type isn't parametric, comparing
two elements of that type will always return True. In fact, we could define 
the instance ignoring the values:

instance == UNIT where
	(==) _ _ = True

1.2: The CONST type is there to add constructor name, which comes handy when 
we want to print the data, but it doesn't affect the actual values, hence
we shouldn't compare the names.

1.3: 
Leaf != [] (in their generic representation) because they have different
daya types (the expression doesn't even type checks). The generic reprresentation
of Leaf has a type of BinG and the generic representation of [] has a type of
ListG. Besides that, their constructor name (string) doesn't match. In addition,
 == isn't defined for those data types.

*/