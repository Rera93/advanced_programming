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

test1 = 
	global \p1 = 0 In
	global \p2 = 0 In
	If (isPressed B1)  
		(p1 =. p1 +. lit 1)
		(print "button b1 not pressed") :.
	If (isPressed B2)
		(p2 =. p2 +. lit 1)
		(print "button b2 not pressed") :.
	If (isPressed B3)
		(p1 =. lit 0)
		(print "button b3 not pressed") 

test2 = 
	global \lastT = 0 In
	periodic lastT (lit 5) (print "Hello")


writeToFile :: String *Files -> *Files
writeToFile c files 
	# (openok,file,files) = fopen "arduino/arduino.ino" FWriteText files
	| not openok = abort "Couldnt open file"
	# file = fwrites c file
	  (closeok,files) = fclose file files
	| not closeok = abort "Coudlnt close file"
	| otherwise = files

// C code generation

Start world = appFiles (writeToFile (compile test2)) world


// Simulation - not working 

// sim :: (Eval a b) -> Task State
sim f = forever $ set (eval f) simState 
	>>| (updateSharedInformation "State" [] simState)

mMemoryShare :: String a -> Shared a | iTask a
mMemoryShare s d = sdsFocus s $ memoryStore s $ Just d

simState :: Shared State
simState = sharedStore "sim_state" zero

