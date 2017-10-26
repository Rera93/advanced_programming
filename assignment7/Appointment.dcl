definition module Appointment

import iTasks
from AUser import :: AUser

:: Appointment = {	
	title :: String,
	when :: DateTime,
	duration :: Time,
	owner :: AUser,
	participants :: [AUser]}

derive class iTask Appointment