// Matheus Amazonas Cabral de Andrade
// s4605640

module skeleton5

import iTasks

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming. Skeleton for assignment 5
 -	use this a project with environment iTasks
 -	executable must be in Examples/iTasks or a subdirectory
	You can also use the -sdk commandline flag to set the path
 -	check Project Options -> Profiling -> Dynamics to prevent recompilation
*/

:: Student =
	{ sname :: String
	, snum :: Int
	, bama :: BaMa
	, year :: Int
	}

:: BaMa = Bachelor | Master

derive class iTask Student, BaMa

instance toString BaMa where
	toString Bachelor = "Bachelor"
	toString Master = "Master"

task1 :: Task Student
task1 = enterInformation "Enter a student" []

task2 :: Task [Student]
task2 = enterInformation "Enter a lis tof students" []

task3 :: Task Student
task3 = updateInformation "Update the student" [] student

task4 :: Task Student
task4 = enterChoice "Choose your favorite student" [] students

task5 :: Task Student
task5 = enterChoice "Choose your favorite student" [ChooseFromDropdown (\s -> s.sname)] students

task6 :: Task Student
task6 = enterChoice "Choose your favorite student" [ChooseFromDropdown gToString{|*|}] students

task7 :: Task [Student]
task7 = enterMultipleChoice "Choose your partner(s)" [ChooseFromCheckGroup getInfo] students

getInfo :: Student ->  String
getInfo {sname, bama} = sname +++ ", " +++ (toString bama) +++ " student"

// Even though this works, I don't like this solution. The ideal solution, in my opinion, would 
// involve only 1 subtask with a custom editor with masking. One alternative was creating 2 tasks
// in parallel: one for updating the student's name and another one to display the other fields.
// The visual layout of the solution was as bad is this one.
task8 :: Task Student
task8 = updateInformation "Update Student's Name" [UpdateAs (\s -> s) (\s1 s2 = {s1 & sname = s2.sname })] student

Start :: *World -> *World
Start w = startEngine task7 w

student = {sname = "Matheus", snum = 738273, bama=Master, year = 2}

students :: [Student]
students =
	[{sname = "Alice"
	 ,snum = 1000
	 ,bama = Master
	 ,year = 1
	 }
	,{sname = "Bob"
	 ,snum = 1003
	 ,bama = Master
	 ,year = 1
	 }
	,{sname = "Carol"
	 ,snum = 1024
	 ,bama = Master
	 ,year = 2
	 }
	,{sname = "Dave"
	 ,snum = 2048
	 ,bama = Master
	 ,year = 1
	 }
	,{sname = "Eve"
	 ,snum = 4096
	 ,bama = Master
	 ,year = 1
	 }
	,{sname = "Frank"
	 ,snum = 1023
	 ,bama = Master
	 ,year = 1
	 }
	]

generic gToString a :: a -> String

gToString {|Student|} {sname} = sname

instance + String where + s t = s +++ t
