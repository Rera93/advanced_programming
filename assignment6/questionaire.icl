// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module questionaire

import iTasks, Data.List

// ------ Derives and instances ------

derive class iTask Question

instance == Question where
	== qa qb = qa.question == qb.question

// ------ Main functions ------

Start :: *World -> *World
Start w = startEngine login w

login :: Task [Question]
login = enterInformation "Enter a username" [] 
	>>* [OnAction ActionOk (hasValue (\u -> redirect u shared))]
	where
		shared :: Shared [Question]
		shared = sharedStore "questions" defaultValue
		redirect u 
			| isTeacher u = teacherHome u
			| isAdmin u = adminHome u
			= studentHome u

// ------ User home functions ------

teacherHome :: UserName (Shared [Question]) -> Task [Question]
teacherHome u s = viewInformation ("Hello, teacher " +++ u) [] ""
	||- enterChoiceWithShared "Choose an item do edit" [ChooseFromGrid id] s 
	>>* [OnAction (Action "Append") (hasValue (append s)),
		 OnAction ActionDelete (hasValue (del s)),
		 OnAction ActionEdit (hasValue (edit u s)),
		 OnAction (Action "First") (always (first s)),
		 OnAction (Action "Clear") (hasValue (\q -> replace q s defaultValue)),
		 OnAction (Action "Quit") (always login)]
	>>= \_ -> teacherHome u s

adminHome :: UserName (Shared [Question]) -> Task [Question]
adminHome u s = viewInformation ("Hello, admin " +++ u) [] "" 
	||- updateSharedInformation "Edit the questions" [] s

// I fetch the list of questions and then display them. 
// I don't call xxxWithShared for each question. I don't
// know how to do that. Lenses, maybe?
studentHome :: UserName (Shared [Question]) -> Task [Question]
studentHome u s = get s 
	>>= \qs -> sequence "student_answers" (map displayQuestion qs)
	>>= \results -> (viewInformation "Correct answers:" [] (count results)
	||- viewInformation "Wrong answers: " [] ((length qs) - (count results)))
	>>| login
	where 
		count rs = foldr (\r a -> if r (a+1) a) 0 rs

displayQuestion :: Question -> Task Bool 
displayQuestion q =  viewInformation "Question:" [] q.question
	||- enterChoice "Select answer" [ChooseFromGrid id] q.choices
	>>= \a -> return (Just(q.answer) == elemIndex a q.choices)

// ------ User roles helper functions ------

isTeacher :: String -> Bool
isTeacher n = n == "Rinus" || n == "Pieter"

isAdmin :: String -> Bool
isAdmin n = n == "admin"
		
// ------- Teacher functions -------

append :: (Shared [Question]) Question -> Task [Question]
append s q = upd (\qs -> appendList q qs) s

del :: (Shared [Question]) Question -> Task [Question]
del s q = upd (\qs -> delete q qs) s

edit :: String (Shared [Question]) Question -> Task [Question]
edit u s q = updateInformation "Edit Question" [] q
	>>* [OnAction ActionOk (hasValue add),
		 OnAction ActionCancel (always (teacherHome u s))]
	where
		add nq = replace q s nq
			>>= \_ -> teacherHome u s

first :: (Shared [Question]) -> Task [Question]
first s = upd (\qs -> [nq:qs]) s
	where 
		nq = { question = "", choices = [], answer = 0}

replace :: Question (Shared [Question]) Question -> Task [Question]
replace oq s nq = upd (\qs -> replaceList oq qs nq) s

// --- List helper functions ---

appendList :: Question [Question] -> [Question]
appendList _ [] = []
appendList q [x:xs]
	| q == x = [x,defaultValue:xs]
	= [x:appendList q xs]
replaceList :: Question [Question] Question -> [Question]
replaceList _ [] _ = []
replaceList oq [x:xs] nq 
	| x == oq = [nq:xs]
	= [x:replaceList oq xs nq]


