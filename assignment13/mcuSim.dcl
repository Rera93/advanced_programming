// Matheus Amazonas Cabral de Andrade
// s4605640

definition module mcuSim

import mcu
import Data.Either
from Data.Map import :: Map
import qualified Data.Map as M
import iTasks
import util

:: Eval a p = Eval ((RW a) State -> (Either String a, State))
:: RW a = R | W a
:: State = { map :: MyMap Int Dynamic,
			 vars :: Int,
			 buttons :: MyMap Button Bool}

instance expr Eval
instance var Eval
instance button Eval 

instance zero State

eval :: (Eval a p) -> State | type a
