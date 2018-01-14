// Matheus Amazonas Cabral de Andrade
// s4605640

definition module mcuCode

import mcu

:: Code a p = Code (CodeState -> CodeState)
:: CodeArea = Include | Global | Setup | Loop
:: CodeState = { area :: CodeArea,
				 lastArea :: CodeArea,
				 fresh :: Int,
				 identI :: Int,
				 include :: [String],
				 identG :: Int,
				 global :: [String],
				 identS :: Int,
				 setup :: [String],
				 identL :: Int,
				 loop :: [String]}

instance expr Code 
instance var Code 
instance button Code 

instance zero CodeState

compile :: (Code a p) -> String
