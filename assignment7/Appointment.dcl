definition module Appointment

import iTasks
from iTasks.Extensions.DateTime import :: DateTime, :: Time
from iTasks.Extensions.Admin.UserAdmin import :: UserAccount

:: Appointment = {	
	title :: String,
	when :: DateTime,
	duration :: Time,
	owner :: User,
	participants :: [User]}

derive class iTask Appointment