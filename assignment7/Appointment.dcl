// Matheus Amazonas Cabral de Andrade
// s4605640

definition module Appointment

import iTasks
from iTasks.Extensions.DateTime import :: DateTime, :: Time

:: Appointment = {	
	title :: String,
	start :: DateTime,
	duration :: Time,
	owner :: User,
	participants :: [User]}

derive class iTask Appointment

makeAppointment :: Task [Appointment]
showAppointments :: Task [Appointment]
createAppointment :: Appointment -> Task [Appointment]
