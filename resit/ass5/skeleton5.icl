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
	{ name :: String
	, snum :: Int
	, bama :: BaMa
	, year :: Int
	}

:: BaMa = Bachelor | Master

Start w = startEngine task8 w

students :: [Student]
students =
	[{name = "Alice"
	 ,snum = 1000
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Bob"
	 ,snum = 1003
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Carol"
	 ,snum = 1024
	 ,bama = Master
	 ,year = 2
	 }
	,{name = "Dave"
	 ,snum = 2048
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Eve"
	 ,snum = 4096
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Frank"
	 ,snum = 1023
	 ,bama = Master
	 ,year = 1
	 }
	]

student :: Student
student = { name = "Alice"
           ,snum = 1000
           ,bama = Master
           ,year = 1 }

generic gToString a :: a -> String
derive class iTask Student, BaMa

instance + String where + s t = s +++ t

gToString{|BaMa|} Bachelor = "Bachelor"
gToString{|BaMa|} Master = "Master"
gToString{|Student|} s = "Name: " +++ s.Student.name 
                +++ ", Student Number: " +++ toString s.snum
                +++ ", Degree: " +++ gToString{|*|} s.bama
                +++ ", Year: " +++ toString s.year

task1 :: Task Student
task1 = enterInformation "Enter a new Student" []

task2 :: Task [Student]
task2 = enterInformation "Enter a list of Students" []

task3 :: Task Student
task3 = updateInformation "Update student" [] student

task4 :: Task Student
task4 = enterChoice "Choose a student" [] students

task5 :: Task Student
task5 = enterChoice "Choose a student" [ChooseFromCheckGroup (\{Student|name} -> name)] students

task6 :: Task Student
task6 = enterChoice "Choose a student" [ChooseFromCheckGroup gToString{|*|}] students

task7 :: Task Student
task7 = enterChoice "Choose a partner" [ChooseFromCheckGroup display] students
where
	display :: Student -> String
	display {name, bama} = name +++ ", " +++ gToString{|*|} bama

task8 :: Task Student
task8 = updateInformation "Change the name" [UpdateAs (\{Student|name} -> name) (\s n -> {Student|s & name = n})] student






