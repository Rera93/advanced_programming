implementation module question2

import question2
import StdEnv, StdList
import Data.Maybe
import qualified Data.Map as Map

// ---------- 2a ----------

next :: Parse String
next = Parse \s -> case s.input of
		[] -> (Nothing, s)
		[a:as] -> (Just a, {s & input = as, seen = [a:s.seen]})

back :: Parse a
back = Parse \s -> case s.seen of
	[] -> (Nothing, s)
	[a:as] -> (Nothing, {s & input = [a:s.input], seen = as})

setGram :: Name Gram -> Parse Gram
setGram n g = Parse \s -> (Just g, {s & store = 'Map'.put n g s.store})

getGram :: Name -> Parse Gram
getGram n = Parse \s -> let g = 'Map'.get n s.store
						in (g, s)

// ---------- 2b ----------

unP :: (Parse a) -> State -> (Maybe a, State)
unP (Parse a) = a

instance Functor Parse where
	fmap f (Parse p) = Parse \s -> case p s of
						(Just x, s) = (Just (f x), s)
						(Nothing, s) = (Nothing, s)

instance Applicative Parse where
	pure x = Parse \s -> (Just x, s)
	(<*>) (Parse pf) (Parse pa) = Parse \s -> case pf s of
						(Just f, s) = let (a, s) = pa s
									   in (fmap f a, s)
						(Nothing, s) = (Nothing, s)

instance Alternative Parse where
	empty = Parse \s -> (Nothing, s)
	(<|>) (Parse pa) (Parse pb) = Parse \s -> case pa s of
						(Just a, s) = (Just a, s)
						(Nothing, s) = pb s

(>>>=) infixl 1 :: (Parse a) (a -> Parse b) -> Parse b
(>>>=) (Parse pa) f = Parse \s -> case pa s of
						(Just a, s) = (unP (f a)) s 
						_ = (Nothing, s)

(>>>|) infixl 1 :: (Parse a) (Parse b) -> Parse b 
(>>>|) p q = p >>>= \_ -> q

guard b :== if b (pure undef) empty

(?) infix 1 :: Bool (v a) -> (v a) | Alternative v 
(?) b m = if b m empty

// ---------- 2c ----------

parse :: Gram -> Parse TREE
parse (Lit s1) = next >>>= \s2 -> if (s1 == s2) (pure (LIT s1)) back
parse Idn = next >>>= \s -> if (isAlpha s.[0]) (pure (IDN s)) back
parse Int = next >>>= \s -> let i = toInt s in if (toString i == s) (pure (INT i)) back
parse (Seq [g:[]]) = parse g >>>= \t -> pure (SEQ [t])
parse (Seq [g:gs]) = parse g >>>= \t1 -> parse (Seq gs) >>>= \(SEQ t2) -> pure (SEQ [t1:t2]) 
parse (Alt [g:[]]) = parse g
parse (Alt [g:gs]) = parse g <|> parse (Alt gs)
parse (Def n g1 g2) = setGram n g1 >>>| parse g2
parse (Var n) = getGram n >>>= \g -> parse g

listIntGram :: Gram
listIntGram = Def "list" (Alt [Lit "[]",Seq [Lit "[",Int,Lit ":",Var "list",Lit "]"]]) 
			  (Seq [Idn,Lit "=",Var "list"])

listIntInput :: [String]
listIntInput = ["mylist","=","[","123",":","[]","]"]

newState :: [String] -> State
newState i = { input = i, seen = [], store = 'Map'.newMap}

// Start = fst ((unP (parse listIntGram)) sZ)
// 	where
// 		sZ = {input = listIntInput, seen = [], store = 'Map'.newMap}









