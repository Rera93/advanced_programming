// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module Proposal

import iTasks
import Appointment
import Util
import iTasks.Extensions.DateTime 
from Data.Func import $
from Data.List import replaceInList, find

derive class iTask Proposal

instance == Proposal where
	(==) x y = x.pid == y.pid

proposals :: Shared [Proposal]
proposals = sharedStore "proposals" []

showProposals :: Task [Proposal]
showProposals = viewSharedInformation ("Open proposals") [] proposals

makeProposal :: Task [Proposal]
makeProposal = get currentUser
		>>= \u -> forever $ enterInformation "Title" []
		-&&- updateInformation "Suggested Start Times" [] []
		-&&- updateInformation "Duration" [] defaultDuration
		-&&- viewInformation "Owner" [ViewAs toString] u
		-&&- selectUsers
		>>* [OnAction (Action "Create") (hasValue createProposal),
			 OnAction ActionCancel (always (return defaultValue))] // TODO: Check what to do here
	where 
		createProposal :: (String, ([DateTime], (Time, (User, [User])))) -> Task [Proposal]
		createProposal (t,(ss,(d,(o,par))))
			# starts = map (\dt -> (dt, [])) ss
			# np = { pid = 0, ptitle = t, pstarts = starts, pduration = d, powner = o, pparticipants = par}
			= 	getNextId
				>>= \i -> upd (\ps -> ps ++ [{np & pid = i}]) proposals
				>>| assignToMany (fillProposal (np)) par
				>>* [OnValue (always (o @: editProposal (np)))] 

fetchProposal :: Proposal -> Task (Maybe Proposal)
fetchProposal p = get proposals
		>>* [OnValue (hasValue (findProposal p))]
	where
		findProposal p ps = return $ find ((==) p) ps

editProposal :: Proposal -> Task [Proposal]
editProposal p = fetchProposal p
		>>= \mp -> case mp of
			Just np -> edit np
			_ -> viewInformation "Couldn't fetch the proposal" [] []
	where
		edit :: Proposal -> Task [Proposal]
		edit p = viewInformation "ID" [] p.pid
				-&&- viewInformation "Title" [] p.ptitle
				-&&- enterChoice "Available start times and users" [ChooseFromCheckGroup id] p.pstarts
				-&&- viewInformation "Duration" [] p.pduration
				-&&- viewInformation "Owner" [] p.powner
				-&&- viewInformation "Participants" [] p.pparticipants
				>>* [OnAction (Action "Create Appointment") (hasValue turnIntoApp),
					 OnAction (Action "Cancel Proposal") (hasValue (deleteProposal o fst))]
			where
				turnIntoApp (_,(t,(s,(d,(o,par)))))
					# na = { aid = 0, title = t, start = fst s, duration = d, owner = o, participants = par}
					 = createAppointment na >>| viewInformation "Appointment created" [] []
				deleteProposal :: Int -> Task [Proposal]
				deleteProposal i = upd (removeFromList (\p -> p.pid == i)) proposals


fillProposal :: Proposal -> Task Proposal
fillProposal p = forever $ get currentUser
		>>= \u -> viewInformation "Title" [] p.ptitle
		||- (enterMultipleChoice "Choose available start times" [ChooseFromCheckGroup id] (map fst p.pstarts)
		-|| viewInformation "Duration" [] p.pduration
		-|| viewInformation "Owner" [] (toString p.powner)
		-|| viewInformation "Participants" [] (map toString p.pparticipants))
		>>* [OnAction ActionOk (hasValue (updateProposal u p))]
	where
		updateProposal :: User Proposal [DateTime] -> Task Proposal
		updateProposal u p sts = upd (\ps -> map (updateStarts u p sts) ps) proposals 
				>>| viewInformation "The proposal was updated" [] "Press \"Continue\" to edit it again"
				>>* [OnAction ActionOk (always (return p))]
			where 
				updateStarts :: User Proposal [DateTime] Proposal -> Proposal
				updateStarts u np sts op 
					| sameProposal np op = {op & pstarts = addStarts op.pstarts sts u}
					| otherwise = op
					where
						addStarts :: [(DateTime, [User])] [DateTime] User -> [(DateTime, [User])]
						addStarts sts ts u = [let users = if (d1 == d2) (addUnique u us) us in (d2, users) \\ (d2,us) <- sts, d1 <- ts]
						sameProposal :: Proposal Proposal -> Bool
						sameProposal p1 p2 = gEq{|*|} {p1 & pstarts = []} {p2 & pstarts = []}


