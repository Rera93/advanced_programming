// Matheus Amazonas Cabral de Andrade
// s4605640

module assignment13

import mcu
import mcuSim
import mcuCode

// import StdEnv

test2 = 
	global \p1 = 0 In
	global \p2 = 0 In
	var \v1 = 56 In
	If (isPressed B1)  
		(p1 =. p1 +. lit 1)
		(lit ())


test3 = 
	var \e = True In
	var \s = False In
	var \g = 2321 In
	var \o = [1] In
	e 

test4 = For (lit [1,2,3]) (\x = 1 In x =. x *. lit 2) 

writeToFile :: String *Files -> *Files
writeToFile c files 
	# (openok,file,files) = fopen "output.c" FWriteText files
	| not openok = abort "Couldnt open file"
	# file = fwrites c file
	  (closeok,files) = fclose file files
	| not closeok = abort "Coudlnt close file"
	| otherwise = files

Start world = appFiles (writeToFile (compile test2)) world

// Problems
//	toString [1,2,3] >>>>>>> ""

// Limitations:
// Lists must be of integers


