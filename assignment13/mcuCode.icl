// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module mcuCode

import mcuCode
import StdEnv
from Data.Func import $

instance zero CodeState where
	zero = {fresh = 0, ident = 0, print = []}

show :: a -> Code b c | toString a
show a = Code $ \s -> {s & print = [toString a:s.print]}

unCode :: (Code a b) -> CodeState -> CodeState
unCode (Code f) = f

(+.+) infixl 5 :: (Code a p) (Code b q) -> Code c r
(+.+) (Code f) (Code g) = Code (g o f)

fresh :: (Int -> (Code a p)) -> Code a p
fresh f = Code $ \s -> unCode (f s.fresh) {s & fresh = inc s.fresh}

freshVar :: ((Code b q) -> (Code a p)) -> (Code a p)
freshVar f = fresh (f o \n -> show ("v" + toString n))

ident :: Code a b
ident = Code $ \s -> {s & ident = inc s.ident}

unident :: Code a b
unident = Code $ \s -> {s & ident = s.ident-1}

nl :: Code a b
nl = Code $ \s -> {s & print = [toString ['\n':repeatn (2 * s.ident) ' ']:s.print]}

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
	If b then else = show "if " +.+ b +.+ show " {" +.+ ident +.+ nl
		+.+ then +.+ unident +.+ nl +.+ show "} else {" +.+ ident +.+ nl
		+.+ else +.+ unident +.+ nl +.+ show "}" +.+ nl
	For s f = freshVar $ \v -> let (x In rest) = f v in
		show "for " +.+ v +.+ show " in " +.+ s +.+ show " do {"
		+.+ ident +.+ nl +.+ rest +.+ unident +.+ nl +.+ show "}"
	(:.) e1 e2 = e1 +.+ show ";" +.+ nl +.+ e2 +.+ nl
	
instance var Code where
	(=.) v e = v +.+ show " = " +.+ e
	var f = freshVar $ \v -> let (x In rest) = f v in
		show (type x) +.+ show " " +.+ v +.+ show " = " +.+
		show x +.+ show ";" +.+ nl +.+ rest

instance button Code where
	isPressed b = show "isPressed " +.+ show b +.+ nl

prettyPrint :: (Code a p) -> [String]
prettyPrint (Code f) = let result = f zero in reverse result.print








