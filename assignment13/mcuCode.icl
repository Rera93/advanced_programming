// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module mcuCode

import mcuCode
from Data.Func import $
// from Data.Foldable import class Foldable, concat

instance zero CodeState where
	zero = { area = Loop,
			 lastArea = Loop,
			 fresh = 0,
			 identI = 0,
			 include = [],
			 identG = 0,
			 global = [],
			 identS = 0,
			 setup = [],
			 identL = 0,
			 loop = []}

show :: a -> Code b c | toString a
show a = Code $ \s -> case s.area of
	Include =  {s & include = [toString a:s.include]}
	Global =  {s & global = [toString a:s.global]}
	Setup =  {s & setup = [toString a:s.setup]}
	Loop =  {s & loop = [toString a:s.loop]}

unCode :: (Code a b) -> CodeState -> CodeState
unCode (Code f) = f

(+.+) infixl 5 :: (Code a p) (Code b q) -> Code c r
(+.+) (Code f) (Code g) = Code (g o f)

fresh :: (Int -> (Code a p)) -> Code a p
fresh f = Code $ \s -> unCode (f s.fresh) {s & fresh = inc s.fresh}

freshVar :: ((Code b q) -> (Code a p)) -> (Code a p)
freshVar f = fresh (f o \n -> show ("v" + toString n))

ident :: Code a b
ident = Code $ \s -> case s.area of
	Include = {s & identI = inc s.identI}
	Global = {s & identG = inc s.identG}
	Setup = {s & identS = inc s.identS}
	Loop = {s & identL = inc s.identL}

unident :: Code a b
unident = Code $ \s -> case s.area of
	Include = {s & identI = dec s.identI}
	Global = {s & identG = dec s.identG}
	Setup = {s & identS = dec s.identS}
	Loop = {s & identL = dec s.identL}

setArea :: CodeArea -> Code a b
setArea a = Code $ \s -> {s & lastArea = s.area, area = a}

restoreArea :: Code a b
restoreArea = Code $ \s -> {s & area = s.lastArea}

nl :: Code a b
nl = Code $ \s -> case s.area of
	Include = {s & include = [toString ['\n':repeatn (2 * s.identI) ' ']:s.include]}
	Global = {s & global = [toString ['\n':repeatn (2 * s.identG) ' ']:s.global]}
	Setup = {s & setup = [toString ['\n':repeatn (2 * s.identS) ' ']:s.setup]}
	Loop = {s & loop = [toString ['\n':repeatn (2 * s.identL) ' ']:s.loop]}

brac :: (Code a p) -> (Code b q)
brac e = show "(" +.+ e +.+ show ")"

instance expr Code where
	lit a = show a
	(+.) x y = brac $ x +.+ show "+" +.+ y
	(-.) x y = brac $ x +.+ show "-" +.+ y
	(*.) x y = brac $ x +.+ show "*" +.+ y
	(&.) x y = brac $ x +.+ show "&" +.+ y
	(|.) x y = brac $ x +.+ show "|" +.+ y
	(~.) x = brac $ show "~" +.+ x
	(==.) x y = brac $ x +.+ show "==" +.+ y
	(!=.) x y = brac $ x +.+ show "!=" +.+ y
	(>.) x y = brac $ x +.+ show ">" +.+ y
	(<.) x y = brac $ x +.+ show "<" +.+ y
	(>=.) x y = brac $ x +.+ show ">=" +.+ y
	(<=.) x y = brac $ x +.+ show "<=" +.+ y
	(+=) e s = e +.+ show " += " +.+ s
	(-=) e s = e +.+ show " -= " +.+ s
	sizeOf l = show "sizeOf(" +.+ l +.+ show ")"
	If b then else = show "if (" +.+ b +.+ show ") {" +.+ ident +.+ nl
		+.+ then +.+ unident +.+ nl +.+ show "} else {" +.+ ident +.+ nl
		+.+ else +.+ unident +.+ nl +.+ show "}" +.+ nl
	(:.) e1 e2 = e1 +.+ nl +.+ e2 +.+ nl
	periodic c e1 e2 = show "if (millis() - " +.+ e1 +.+ show " > " +.+ c +.+ show") {" +.+ ident +.+ nl
		+.+ e2 +.+ nl
		+.+ c +.+ show " += " +.+ e1 +.+ show ";" +.+ unident +.+ nl +.+ show "}" 
	
instance var Code where
	(=.) v e = v +.+ show " = " +.+ e +.+ show ";"
	var f = freshVar $ \v -> let (x In rest) = f v in
		show (type x) +.+ show " " +.+ v +.+ show " = " +.+
		show x +.+ show ";" +.+ nl +.+ rest
	global f = freshVar $ \v -> let (x In rest) = f v in
		setArea Global +.+ show (type x) +.+ show " " +.+ v +.+ show " = " +.+
		show x +.+ show ";" +.+ nl +.+ restoreArea +.+ rest

instance button Code where
	isPressed b = show "isPressed(" +.+ show b +.+ show ")"
	pressed b = show ""

instance lcd Code where
	print s = show "lcd.print(\"" +.+ show s +.+ show "\");"

concat :: [String] -> String
concat [] = ""
concat [a:as] = a +++ concat as

compile :: (Code a p) -> String
compile f = concat $ let r = (unCode final) zero in (reverse r.include) ++ (reverse r.global) ++ (reverse r.setup) ++ (reverse r.loop)
	where
		final = include +.+ global +.+ isPressCode +.+ setup +.+ loop +.+ f +.+ unident +.+ nl +.+ show "}"
		include = setArea Include 
			+.+ show "#include <LiquidCrystal.h>" +.+ nl 
			+.+ show "#define KEY_COUNT 5" +.+ nl 
		global = setArea Global +.+ nl 
			+.+ show "int keyLimits [KEY_COUNT+1] = {50, 190, 380, 555, 790, 1024};" +.+ nl 
			+.+ show "char keyNames [KEY_COUNT+1] [10] = {\"Right \", \"Up \", \"Down \" , \"Left \" , \"Select\" , \"No key\"};" 
			+.+ nl +.+ show "LiquidCrystal lcd = LiquidCrystal(8, 9, 4, 5, 6, 7);" +.+ nl  +.+ nl
		setup = setArea Setup +.+ nl 
			+.+ show "void setup() {" +.+ ident +.+ nl 
			+.+ show "lcd.begin(16,2);"
			+.+ unident +.+ nl +.+ show "}" +.+ nl +.+ nl +.+ restoreArea
		loop = setArea Loop +.+ nl
			+.+ show "void loop() {" +.+ ident +.+ nl 
			+.+ show "lcd.setCursor(0, 0);" +.+ nl
		isPressCode = show "boolean isPressed (int button){" +.+ ident +.+ nl
			+.+ show "int val = analogRead(A0);" +.+ nl
			+.+ show "for (int i = 0; i <= KEY_COUNT; i += 1) {" +.+ ident +.+ nl
			+.+ show "if (val < keyLimits[i] && i == button) {" +.+ ident +.+ nl
			+.+ show "return true;" +.+ unident +.+ nl
			+.+ show "}" +.+ unident +.+ nl
			+.+ show "}" +.+ nl
			+.+ show "return false;" +.+ unident +.+ nl
			+.+ show "}" +.+ nl +.+ nl










