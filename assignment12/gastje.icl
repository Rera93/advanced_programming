implementation module gastje

/*
	Pieter Koopman, Radboud University, 2016, 2017
	pieter@cs.ru.nl
	Advanced programming
	A simplified MBT tool based on logical properties
	
	Use the iTask environment!
	Execute with "Basic values only" option
*/

import StdEnv, StdGeneric, GenEq, Data.Eq, Data.Maybe, StdOverloaded

test :: p -> [String] | prop p
test p = check 1000 (holds p prop0)

check :: Int [Prop] -> [String]
check n [] = ["Proof\n"]
check 0 l  = ["Passed\n"]
check n [p:x] | p.bool
	= check (n-1) x
	= ["Fail for: ":reverse ["\n":p.info]]

(For) infix 6 :: (a->b) [a] -> ((a->b),[a]) | prop b & testArg a
(For) p as = (p, as)

(==>) infix 1 :: Bool b -> Maybe b
(==>) a b = if a (Just b) Nothing

(=.=) infix 5 :: a a -> Prop | testArg a
(=.=) a b = {bool = gEq{|*|} a b, info = [ string{|*|} a, " =.=: ", string{|*|} b]}

class prop a where holds :: a Prop -> [Prop]

instance prop Bool where holds b p = [{p & bool = b}]

// crashes the iTask compiler if there is no dcl module :(
instance prop (a->b) | prop b & testArg a 
where
	holds f p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- gen{|*|}]

instance prop ((a->b),[a]) | prop b & testArg a
where
	holds (f, as) p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- as]

instance prop (Maybe a) | prop a
where
	holds (Just b) p = holds b p
	holds _ p = [p]

instance prop Prop 
where
	holds p1 p2 = [{p2 & bool = p1.bool && p2.bool, info = p2.info ++ p1.info}]

:: Prop =
	{ bool :: Bool
	, info :: [String]
	}
prop0 = {bool = True, info = []}

generic gen a :: [ a ]
gen{|Int|}  = [0,1,-1,maxint,minint,maxint-1,minint+1:[j\\i<-[2..], j<-[i,~i]]]
gen{|Bool|} = [True,False]
gen{|Char|} = [' '..'~'] ++ ['\t\n\b']
gen{|UNIT|} = [UNIT]
gen{|PAIR|}   f g	= map (\(a,b)=PAIR a b) (diag2 f g)
gen{|EITHER|} f g = merge (map RIGHT g) (map LEFT f)
where
  merge [a:x] ys = [a: merge ys x]
  merge []    ys = ys
gen{|CONS|}   f  = map CONS f
gen{|OBJECT|} f  = map OBJECT f
gen{|RECORD|} f  = map RECORD f
gen{|FIELD|}  f  = map FIELD f

generic string a :: a -> String
string{|Int|} i = toString i
string{|Bool|} b = toString b
string{|Char|} c = toString ['\'',c,'\'']
string{|UNIT|} _ = ""
string{|PAIR|} f g (PAIR x y) = f x + " " + g y
string{|EITHER|} f g (LEFT x) = f x
string{|EITHER|} f g (RIGHT y) = g y
string{|CONS of gcd|} f (CONS x) | gcd.gcd_arity > 0
	= "(" + gcd.gcd_name + " " + f x + ")"
	= gcd.gcd_name
string{|OBJECT|} f (OBJECT x) = f x
string{|RECORD of grd|} f (RECORD x) = "{" + grd.grd_name + "|" + f x + "}"
string{|FIELD of gfd|} f (FIELD x) = gfd.gfd_name + " = " + f x + " "

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

instance + String where + s t = s +++ t

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

fac :: Int -> Int
fac 7 = 1
fac n
	| n <= 0 = 1
	| otherwise = n * fac (n-1)

pChar c = isAlpha c || toUpper c == c
pUpper :: Char -> Bool
pUpper c = c /= toUpper c

pfac i = abs i < 10 ==> prod [1..i] =.= fac i

pTest :: Int -> Bool
pTest 0 = False
pTest n 
	| isEven n = True
	| otherwise = False

Start = ["pfac: ":test pfac]

// Start = ["pUpper: lower": test (\n -> (isEven n && isNotzero n) ==> pTest n)]
// 	where
// 		isNotzero 0 = False
// 		isNotzero _ = True


