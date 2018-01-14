// Matheus Amazonas Cabral de Andrade
// s4605640

module assignment13

import mcu
import mcuSim
import mcuCode
import iTasks
from Data.Func import $
import iTasks.Internal.Store


derive class iTask State, Button

// import StdEnv

test2 = 
	global \p1 = 0 In
	global \p2 = 0 In
	If (isPressed B1)  
		(p1 =. p1 +. lit 1)
		(lit ()) :.
	If (isPressed B2)
		(p2 =. p2 +. lit 1)
		(lit ()) :.
	If (isPressed B3)
		(p1 =. lit 0)
		(lit ())


test3 = 
	var \e = True In
	var \s = False In
	var \g = 2321 In
	var \o = [1] In
	e 

writeToFile :: String *Files -> *Files
writeToFile c files 
	# (openok,file,files) = fopen "output.c" FWriteText files
	| not openok = abort "Couldnt open file"
	# file = fwrites c file
	  (closeok,files) = fclose file files
	| not closeok = abort "Coudlnt close file"
	| otherwise = files

// sim :: (Eval a b) -> Task State
sim f = forever $ set (eval f) simState 
	>>| (updateSharedInformation "State" [] simState)

mMemoryShare :: String a -> Shared a | iTask a
mMemoryShare s d = sdsFocus s $ memoryStore s $ Just d

simState :: Shared State
simState = sharedStore "sim_state" zero

// Start = eval test3

// Start world = appFiles (writeToFile (compile test2)) world



Start :: *World -> *World
Start w = startEngine (sim test2) w

// Problems
//	toString [1,2,3] >>>>>>> ""

// Limitations:
// Lists must be of integers


