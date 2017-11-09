// Matheus Amazonas Cabral de Andrade
// s4605640

definition module Appointment

import iTasks
from iTasks.Extensions.DateTime import :: DateTime, :: Time

:: Appointment = {	
	aid :: Int,
	title :: String,
	start :: DateTime,
	duration :: Time,
	owner :: User,
	participants :: [User]}

derive class iTask Appointment
instance == Appointment

makeAppointment :: Task [Appointment]
showAppointments :: Task [Appointment]
createAppointment :: Appointment -> Task [Appointment]
