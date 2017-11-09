// Matheus Amazonas Cabral de Andrade
// s4605640

definition module Proposal

import iTasks
from iTasks.Extensions.DateTime import :: DateTime, :: Time

:: Proposal = {
	ptitle :: String,
	pstarts :: [(DateTime, [User])],
	pduration :: Time,
	powner :: User,
	pparticipants :: [User]}

derive class iTask Proposal

showProposals :: Task [Proposal]
fillProposal :: Proposal -> Task Proposal
makeProposal :: Task [Proposal]