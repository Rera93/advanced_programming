implementation module question4

import question2, question4
import StdEnv
import Data.Maybe
from Data.Func import $

// 4a is in the definition file

// ---------- 4b ----------

instance gram (Parse TREE) where
	lit s1 = next >>>= \s2 -> if (s1 == s2) (pure (LIT s1)) back
	idn = next >>>= \s -> if (isAlpha s.[0]) (pure (IDN s)) back
	int = next >>>= \s -> let i = toInt s in if (toString i == s) (pure (INT i)) back
	sequ [g:[]] = g >>>= \t -> pure (SEQ [t])
	sequ [g:gs] = g >>>= \t1 -> sequ gs >>>= \(SEQ t2) -> pure (SEQ [t1:t2])
	alt [g:[]] = g
	alt [g:gs] = g <|> alt gs
	def f = let (p,q) = f p in q            // WTH is this?????????? From the solution

listIntGramS :: v | gram v 
listIntGramS = def \list -> (alt [lit "[]", sequ [lit "[", int, lit ":", list, lit "]"]],
							 sequ [idn, lit "=", list])

parseListIntGramS :: Parse TREE
parseListIntGramS = listIntGramS

// Start = fromJust $ fst $ ((unP parseListIntGramS) (newState listIntInput))