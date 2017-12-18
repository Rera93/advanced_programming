// Matheus Amazonas Cabral de Andrade
// s4605640

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
import cashModel

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

// ---------- Cash Model ----------

derive gen Euro, Action, Product, []
derive string Euro, Action, Product, []
derive bimap []
derive gEq Action, Product

// --- Euro properties ---

pAlwaysConvertAdd e1 e2 = let s = e1 + e2 in s.cent < 100
// output: Passed

pAlwaysConvertSub e1 e2 = let s = e1 - e2 in s.cent < 100
// output: Passed

pSignal e = case sign e.euro of
	1  = sign e.cent /= -1
	-1 = sign e.cent /= 1
	otherwise = True
// output: Fail for euro = -1, cent = 1

pTotalAdd e1 e2 = let s = e1 + e2 in toCent s =.= toCent e1 + toCent e2
	where
		toCent e = e.euro * 100 + e.cent
// output: Fail for {euro = 0, cent = 0} + {euro = 0, cent = 1}
// =.= output: 1 =.= 0

pTotalSub e1 e2 = let s = e1 - e2 in toCent s =.= toCent e1 - toCent e2
	where
		toCent e = e.euro * 100 + e.cent
// output: Fail for {euro = 0, cent = 0} + {euro = 0, cent = 1}
// =.= output: -1 =.= 0

pAsso :: Euro Euro Euro -> Prop
pAsso e1 e2 e3 = e1 + (e2 + e3) =.= (e1 + e2) + e3
// output: Fail for {euro = 1, cent = -1} {euro = 0, cent = 0}, {euro = 0, cent = 0}
// =.= output: {euro = 0, cent = 0} =.= {euro = 0, cent = 99}

pInv e = case sign e.euro of
	1  = sign (~e).euro =.= -1 
	-1 = sign (~e).euro =.= 1
	0  = sign (~e).euro =.= 0
// output: Fail for {euro = -9223372036854775808, cent = 0}
// =.= output: 1 =.= -1

// --- Remove action properties ---

pTotalPositive ps a = total.euro >= 0 && total.cent >= 0
	where
		total = sum (snd (model ps a))
// output: Fail for [] (Rem Cola)

pRemoved :: [Product] Action Product -> Prop
pRemoved ps a p = total (prods ps a) (Rem p) =.= total ps a - euro p
	where 
		total x y = sum (snd (model x y))
		prods x y = fst (model x y)
// output: Fail for [] Pay Cola
// =.= output: {euro = -1, cent = 0} =.= {euro = 0, cent = 0}


Start = test pRemoved

