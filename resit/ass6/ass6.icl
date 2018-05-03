module ass6

import iTasks
import StdChar
import StdMisc
import Data.Array

:: Question = { 
	question :: String,
	answers :: [String],
	correct :: Int }

Start w = startEngine mainTask w

derive class iTask Question

mainTask :: Task [Question]
mainTask =  let sh = sharedStore "questions" [] in
	enterInformation "Username" []
	>>= \u -> case  u of
		"rinus" = teacherTask sh 
		"peter" = teacherTask sh 
		"admin" = adminTask sh 
		_       = userTask sh 
where
	caps :: String -> String
	caps s = mapArr toUpper s

teacherTask :: (Shared [Question]) -> Task [Question]
teacherTask sh = enterChoiceWithShared "Choose a question to manipulate" [ChooseFromGrid snd] (mapRead (\qs -> [(i,q) \\ q <- qs & i <- [0..]]) sh)
	>>* [OnAction (Action "Append") (hasValue (show append)),
	     OnAction ActionDelete (hasValue (show delete)),
	     OnAction ActionEdit (hasValue (show edit)),
	     OnAction (Action "First") (always (show append (-1, defaultValue))),
	     OnAction (Action "Clear") (hasValue (show clear)),
	     OnAction ActionQuit (always (get sh))]
where
	show :: ((Int,Question) -> Task [Question]) (Int,Question) -> Task [Question]
	show t i = t i >>= \_ -> teacherTask sh
	append :: (Int,Question) -> Task [Question]
	append (i,_) = enterInformation "Create new question" []
		>>= \q -> upd (\qs -> let (b,e) = splitAt (i+1) qs in b ++ [q] ++ e) sh
	delete :: (Int,Question) -> Task [Question]
	delete (i,_) = upd (removeAt i) sh
	edit :: (Int,Question) -> Task [Question]
	edit (i,q) = updateInformation "Edit question" [] q
		>>= \q -> upd (updateAt i q) sh
	clear :: (Int,Question) -> Task [Question]
	clear (i,_) = upd (updateAt i defaultValue) sh 

adminTask :: (Shared [Question]) -> Task [Question]
adminTask sh = updateSharedInformation  "Edit questions" [] sh

userTask :: (Shared [Question]) -> Task [Question]
userTask sh = get sh >>= \qs -> sequence "" (map answer qs)
	>>= \res -> viewInformation (correct res) []  []
where
	answer :: Question -> Task Bool
	answer q = viewInformation "Question" [] q.question ||-
		enterChoice "Answer" [ChooseFromGrid snd] [(i,a) \\ a <- q.answers & i <- [0..]]
		>>= \(i,_) -> return (i == q.correct)
	correct :: [Bool] -> String
	correct res = let c = sum (map bToI res) in toString c +++ " out of " +++ toString (length res)
	where
		bToI :: Bool -> Int
		bToI False = 0
		bToI _ = 1

