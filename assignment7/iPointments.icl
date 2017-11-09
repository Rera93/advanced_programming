// Matheus Amazonas Cabral de Andrade
// s4605640

module iPointments 

import Appointment
import Proposal
import Util
import iTasks.Extensions.DateTime 
from Data.List import replaceInList, find

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" showAppointments,
		transientWorkflow "Show Proposals" "Show all open proposals" showProposals,
		transientWorkflow "Make Appointment" "Make a new appointment" makeAppointment,
		transientWorkflow "Propose Appointment" "Propose a new appointment" makeProposal,
		restrictedTransientWorkflow "Manage users" "Manage system users" ["admin"] manageUsersSafely]

manageUsersSafely :: Task ()
manageUsersSafely = try manageUsers catchUserError 
	>>* [OnAction ActionOk (always manageUsersSafely)]
	where
		catchUserError :: String -> Task ()
		catchUserError s = manageUsersSafely -|| viewInformation ("Error: " +++ s) [] () 



