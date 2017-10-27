module iPointments 

import Appointment
import iTasks.Extensions.DateTime

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" (showAppointments appointments),
		transientWorkflow "Make Appointment" "Make a new appointment" (makeAppointment appointments),
		transientWorkflow "Propose Appointment" "Propose a new appointment" (makeAppointment appointments),
		restrictedTransientWorkflow "Manage users" "Manage system users" ["admin"] manageUsersSafe]
	where 
		appointments = sharedStore "appointments" defaultValue

manageUsersSafe :: Task ()
manageUsersSafe = try manageUsers catchUserError 
	>>* [OnAction ActionOk (always manageUsersSafe)]
where
	catchUserError :: String -> Task ()
	catchUserError s = manageUsersSafe -|| viewInformation ("Error: " +++ s) [] () 

showAppointments :: (Shared [Appointment]) -> Task [Appointment]
showAppointments s = updateSharedInformation "Future appointments" [] s

makeAppointment :: (Shared [Appointment]) -> Task [Appointment]
makeAppointment s = viewSharedInformation "Make an appointment" [] s

proposeAppointment :: (Shared [Appointment]) -> Task [Appointment]
proposeAppointment s = viewSharedInformation "Propose an appointment" [] s
