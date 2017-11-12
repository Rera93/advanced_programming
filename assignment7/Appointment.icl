// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module Appointment

import iTasks
import Util
import iTasks.Extensions.DateTime
from Data.Func import $
from Data.List import find

derive class iTask Appointment

instance == Appointment where
	(==) x y = x.aid == y.aid

appointments :: Shared [Appointment]		
appointments = sharedStore "appointments" []

// I see 2 problems with using the -&&- here
// First, the web layout is not pleasing to look at, with each field taking 
// too much vertical space. Second, the result is a nested tuple, which isn't 
// the best solution ever. I wish there was a way of currying that.
makeAppointment :: Task [Appointment]
makeAppointment = get currentUser
		>>= \u -> forever $ get currentDateTime
		>>= \curDT -> enterInformation "Title" [] 
		-&&- updateInformation "Start" [] curDT	
		-&&- updateInformation "Duration" [] defaultDuration
		-&&- viewInformation "Owner" [ViewAs toString] u
		-&&- selectUsers 
		>>* [OnAction (Action "Make") (hasValue createAppointmentTup),
			 OnAction ActionCancel (always (return defaultValue))]	// TODO: Check what to do here
	where
		createAppointmentTup :: (String, (DateTime, (Time, (User, [User])))) -> Task [Appointment]
		createAppointmentTup (t,(s,(d,(o,par)))) = createAppointment { aid = 0, title = t, start = s, duration = d, owner = o, participants = par}

createAppointment :: Appointment -> Task [Appointment]
createAppointment a = getNextId 
		>>= \i -> let na = { a & aid = i} in upd (\as -> as ++ [na]) appointments
		>>* [OnValue (always (assign na))]
	where
		assign a = assignToMany (viewAppointment a) a.participants

viewAppointment :: Appointment -> Task Appointment
viewAppointment a = fetchFromShared appointments a
		>>= \ma -> case ma of
			Just na -> viewInformation "Appointment" [] na 
					>>* [OnAction (Action "Done") (hasValue return)]
			_ -> viewInformation "Couldn't fetch the appointment" [] "" >>| return defaultValue

showAppointments :: Task [Appointment]
showAppointments = get currentDateTime
		>>= \now -> viewSharedInformation ("Future appointments") [ViewAs (filter (\a -> a.start > now))] appointments