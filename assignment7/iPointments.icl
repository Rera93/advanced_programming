module iPointments 

import Appointment
import iTasks.Extensions.DateTime 
import Util
import StdDebug
from Data.Func import $

defaultDuration :: Time
defaultDuration = {Time| defaultValue & hour = 1}

Start :: *World -> *World
Start w = startEngine (loginAndManageWorkList "Welcome to iPointments" home) w

home :: [Workflow]
home = [transientWorkflow "Show Appointments" "Show all the future appointments" showAppointments,
		transientWorkflow "Make Appointment" "Make a new appointment" makeAppointment,
		transientWorkflow "Propose Appointment" "Propose a new appointment" proposeAppointment,
		restrictedTransientWorkflow "Manage users" "Manage system users" ["admin"] manageUsersSafely]

appointments :: Shared [Appointment]		
appointments = sharedStore "appointments" []

proposals :: Shared [Proposal]
proposals = sharedStore "proposals" []

manageUsersSafely :: Task ()
manageUsersSafely = try manageUsers catchUserError 
	>>* [OnAction ActionOk (always manageUsersSafely)]
	where
		catchUserError :: String -> Task ()
		catchUserError s = manageUsersSafely -|| viewInformation ("Error: " +++ s) [] () 

selectUsers :: Task [User]
selectUsers = get users >>= \us -> enterMultipleChoice "Select Participants" [ChooseFromCheckGroup id] us

showAppointments :: Task [Appointment]
showAppointments = updateSharedInformation ("Future appointments", "Choose an appointment to view") [] appointments

// I see 2 problems with using the -&&- here
// First, the web layout is awful, with each field taking too much vertical space
// Second, the result is a nested tuple, which isn't the best solution ever
makeAppointment :: Task [Appointment]
makeAppointment = forever $ get currentDateTime
		>>= \curDT -> enterInformation "Title" [] 
		-&&- updateInformation "Start" [] curDT	
		-&&- updateInformation "Duration" [] defaultDuration
		-&&- selectUsers 
		>>* [OnAction (Action "Make") (hasValue createAppointment),
			 OnAction ActionCancel (always (return defaultValue))]	// TODO: Check what to do here
	where
		createAppointment :: (String, (DateTime, (Time, [User]))) -> Task [Appointment]
		createAppointment (t,(s,(d,par))) = get currentUser 
				>>= \u -> upd (\as -> as ++ [na u]) appointments
				>>| assignToMany (viewAppointment (na u)) par 
			where
				na o = { title = t, start = s, duration = d, owner = o, participants = par}
				viewAppointment a = viewInformation "Appointment" [] a

assignToMany :: (Task a) [User] -> Task [a] | iTask a
assignToMany t us = allTasks (map (\u -> u @: t) us) 
		>>* [OnAction ActionOk (always (return defaultValue))]

viewProposal :: Proposal -> Task Proposal
viewProposal p = viewInformation "Title" [] p.ptitle
		||- (enterMultipleChoice "Choose available start times" [ChooseFromCheckGroup id] (map fst p.pstarts)
		-|| viewInformation "Duration" [] p.pduration
		-|| viewInformation "Owner" [] (toString p.powner)
		-|| viewInformation "Participants" [] (map toString p.pparticipants))
		>>* [OnValue (hasValue addUserAvail)]
	where
		addUserAvail sts = return defaultValue //TODO: Implement proposal 

proposeAppointment :: Task [Proposal]
proposeAppointment = forever $ enterInformation "Title" []
		-&&- updateInformation "Suggested Start Times" [] []
		-&&- updateInformation "Duration" [] defaultDuration
		-&&- selectUsers
		>>* [OnAction (Action "Create") (hasValue createProposal),
			 OnAction ActionCancel (always (return defaultValue))] // TODO: Check what to do here
	where 
		createProposal :: (String, ([DateTime], (Time, [User]))) -> Task [Proposal]
		createProposal (t,(ss,(d,par))) = get currentUser 
				>>= \u -> upd (\ps -> ps ++ [np u]) proposals
				>>| assignToMany (viewProposal (np u)) par
			where
				np u = { ptitle = t, pstarts = map (\s -> (s,[])) ss, pduration = d, powner = u, pparticipants = par}

