// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module mcu

import mcu
import StdString


instance toString Button where
	toString B1 = "B1"
	toString B2 = "B2"
	toString B3 = "B3"
	toString B4 = "B4"
	toString B5 = "B5"

instance < Button where
	(<) B1 _  = True
	(<) B2 B1 = False
	(<) B2 _  = True
	(<) B3 B1 = False
	(<) B3 B2 = False
	(<) B3 _  = True
	(<) B4 B1 = False
	(<) B4 B2 = False
	(<) B4 B3 = False
	(<) B3 _  = True
	(<) B5 _  = False

instance type Bool where
	type _ = "bool"

instance type Int where
	type _ = "int"

instance type Char where
	type _ = "char"

instance type [Int] where
	type _ = "[int]"

instance type () where 
	type _ = "()"

instance toString () where
	toString _ = ""

instance + String where
	+ s1 s2 = s1 +++ s2

