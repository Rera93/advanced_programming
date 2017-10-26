module iPointments 

import AUser, Appointment
import iTasks.Extensions.DateTime

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" showAppointments,
		transientWorkflow "Make Appointment" "Make a new appointment" makeAppointment,
		transientWorkflow "Propose Appointment" "Propose a new appointment" makeAppointment]


showAppointments :: Task [Appointment]
showAppointments = updateInformation "Future appointments" [] [] 

makeAppointment :: Task Appointment
makeAppointment = viewInformation "Make an appointment" [] defaultValue

proposeAppointment :: Task Appointment
proposeAppointment = viewInformation "Propose an appointment" [] defaultValue
