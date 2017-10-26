definition module Appointment

import iTasks

:: Appointment = {	
	title :: String,
	when :: DateTime,
	duration :: Time,
	owner :: UserAccount,
	participants :: [UserAccount]}

derive class iTask Appointment