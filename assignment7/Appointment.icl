// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module Appointment

import iTasks
import Util
from Data.Func import $

derive class iTask Appointment

appointments :: Shared [Appointment]		
appointments = sharedStore "appointments" []

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

showAppointments :: Task [Appointment]
showAppointments = updateSharedInformation ("Future appointments", "Choose an appointment to view") [] appointments

