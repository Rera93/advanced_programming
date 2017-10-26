module iPointments 

import Appointment
import iTasks.Extensions.DateTime

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" (showAppointments appointments),
		transientWorkflow "Make Appointment" "Make a new appointment" (makeAppointment appointments),
		transientWorkflow "Propose Appointment" "Propose a new appointment" (makeAppointment appointments),
		restrictedTransientWorkflow "Manage users" "Manage system users" ["admin"] manageUsers]
	where 
		appointments = sharedStore "appointments" defaultValue


showAppointments :: (Shared [Appointment]) -> Task [Appointment]
showAppointments s = updateSharedInformation "Future appointments" [] s

makeAppointment :: (Shared [Appointment]) -> Task [Appointment]
makeAppointment s = viewSharedInformation "Make an appointment" [] s

proposeAppointment :: (Shared [Appointment]) -> Task [Appointment]
proposeAppointment s = viewSharedInformation "Propose an appointment" [] s
