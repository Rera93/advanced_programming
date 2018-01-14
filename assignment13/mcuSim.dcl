// Matheus Amazonas Cabral de Andrade
// s4605640

definition module mcuSim

import mcu
import StdEnv
import Data.Map
import Data.Either

:: Eval a p = Eval ((RW a) State -> (Either String a, State))
:: RW a = R | W a
:: State = { map :: Map Int Dynamic,
			 vars :: Int,
			 buttons :: Map Button Bool}

instance expr Eval
instance var Eval
instance button Eval 

eval :: (Eval a p) -> [String] | type a