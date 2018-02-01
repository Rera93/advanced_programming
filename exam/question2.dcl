definition module question2

import Data.Functor
import Control.Applicative
from StdOverloaded import class zero
from Data.Map import :: Map

:: Gram =
	  Lit String
	| Idn
	| Int
	| Seq [Gram]
	| Alt [Gram]
	| Def Name Gram Gram
	| Var Name
:: Name :== String
:: State = { input :: [String], seen :: [String], store :: Store}
:: Store :== Map Name Gram
:: Parse a = Parse (State -> (Maybe a, State))

:: TREE = LIT String | IDN String | INT Int | SEQ [TREE]

next :: Parse String
back :: Parse a

(>>>=) infixl 1 :: (Parse a) (a -> Parse b) -> Parse b
(>>>|) infixl 1 :: (Parse a) (Parse b) -> Parse b 

instance Functor Parse
instance Applicative Parse
instance Alternative Parse

unP :: (Parse a) -> State -> (Maybe a, State)
parse :: Gram -> Parse TREE
newState :: [String] -> State
listIntGram :: Gram
listIntInput :: [String]