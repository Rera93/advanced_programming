module iPointments 

import Appointment
import iTasks.Extensions.DateTime 
from Data.Func import $

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" (showAppointments appointments),
		transientWorkflow "Make Appointment" "Make a new appointment" (makeAppointment appointments),
		transientWorkflow "Propose Appointment" "Propose a new appointment" (proposeAppointment appointments),
		restrictedTransientWorkflow "Manage users" "Manage system users" ["admin"] manageUsersSafely]
	where 
		appointments = sharedStore "appointments" []

manageUsersSafely :: Task ()
manageUsersSafely = try manageUsers catchUserError 
	>>* [OnAction ActionOk (always manageUsersSafely)]
	where
		catchUserError :: String -> Task ()
		catchUserError s = manageUsersSafely -|| viewInformation ("Error: " +++ s) [] () 

showAppointments :: (Shared [Appointment]) -> Task [Appointment]
showAppointments s = updateSharedInformation ("Future appointments", "Choose an appointment to view") [] s

// I see 2 problems with using the -&&- here
// First, the web layout is awful, with each field taking too much vertical space
// Second, the result is a nested tuple, which isn't the best solution ever
makeAppointment :: (Shared [Appointment]) -> Task [Appointment]
makeAppointment s 
	# defDuration = {Time| defaultValue & hour = 1}
	= forever $ get currentDateTime
		>>= \curDT -> enterInformation "Title" [] 
		-&&- updateInformation "Start" [] curDT	
		-&&- updateInformation "Duration" [] defDuration
		-&&- selectUsers
		>>* [OnAction (Action "Make") (hasValue (saveAppointment s)),
			OnAction ActionCancel (always (makeAppointment s))]
	where
		selectUsers :: Task [User]
		selectUsers = get users >>= \us -> enterMultipleChoice "Select Participants" [ChooseFromCheckGroup id] us
		saveAppointment :: (Shared [Appointment]) (String, (DateTime, (Time, [User]))) -> Task [Appointment]
		saveAppointment sh (t,(s,(d,ps))) = get currentUser >>= \u -> upd (\as -> as ++ [na u]) sh
			where
				na o = { title = t, when = s, duration = d, owner = o, participants = ps}

proposeAppointment :: (Shared [Appointment]) -> Task [Appointment]
proposeAppointment s = updateSharedInformation ("Propose an appointment", "Propose an appointment") [] s
