definition module Appointment

import iTasks
from iTasks.Extensions.DateTime import :: DateTime, :: Time
from iTasks.Extensions.Admin.UserAdmin import :: UserAccount

:: Appointment = {	
	title :: String,
	start :: DateTime,
	duration :: Time,
	owner :: User,
	participants :: [User]}

:: Proposal = {
	ptitle :: String,
	pstarts :: [(DateTime, [User])],
	pduration :: Time,
	powner :: User,
	pparticipants :: [User]}

derive class iTask Appointment, Proposal