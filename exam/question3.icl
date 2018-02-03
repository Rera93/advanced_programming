module question3

import question2
import iTasks
import Data.Maybe
from Data.Func import $

derive class iTask TREE, Gram

//Start :: *World -> Task TREE
Start w = startEngine (mainTask defaultValue defaultValue) w

mainTask :: Gram [String] -> Task (Gram, [String])
mainTask g i = (updateInformation "Select the grammar" [] g 
		  -&&-  updateInformation "Select an input" [] i)
		>>* [OnAction ActionOk (ifValue valid showTree),
			 OnAction (Action "Clear Grammar") (hasValue clearG),
			 OnAction (Action "List Grammar") (always (mainTask listIntGram i)),
			 OnAction (Action "Clear Input") (hasValue clearI),
			 OnAction (Action "myList") (hasValue myList)]
	where 
		valid :: (Gram, [String]) -> Bool
		valid (g,i) = isJust $ fst $ ((unP (parse g)) (newState i))
		showTree :: (Gram, [String]) -> Task (Gram, [String])
		showTree (g, i) = viewInformation "Resulting parse TREE" [] (getTree (g,i))
			>>* [OnAction ActionCancel (always (mainTask defaultValue defaultValue))]
		getTree :: (Gram, [String]) -> TREE
		getTree (g,i) = fromJust $ fst $ ((unP (parse g)) (newState i))
		clearG :: (Gram, [String]) -> Task (Gram, [String])
		clearG (_,i) = mainTask defaultValue i
		clearI :: (Gram, [String]) -> Task (Gram, [String])
		clearI (g,_) = mainTask g defaultValue
		myList :: (Gram, [String]) -> Task (Gram, [String])
		myList (g,_) = mainTask g ["myList","=","[","1",":","[]","]"]



