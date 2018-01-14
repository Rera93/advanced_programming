// Matheus Amazonas Cabral de Andrade
// s4605640

definition module mcuCode

import mcu

:: Code a p = Code (CodeState -> CodeState)
:: CodeState = { fresh :: Int,
				 ident :: Int,
				 print :: [String]}

instance expr Code 
instance var Code 
instance button Code 

instance zero CodeState

prettyPrint :: (Code a p) -> [String]
