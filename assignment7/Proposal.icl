// Matheus Amazonas Cabral de Andrade
// s4605640

implementation module Proposal

import iTasks
import Appointment
import Util
import iTasks.Extensions.DateTime 
from Data.Func import $
from Data.List import replaceInList

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
		>>* [OnAction (Action "Create") (ifValue (not o isEmpty o fst o snd) createProposal),
			 OnAction ActionCancel (always (return defaultValue))] // TODO: Check what to do here. Ask when submitting
	where 
		createProposal :: (String, ([DateTime], (Time, (User, [User])))) -> Task [Proposal]
		createProposal (t,(ss,(d,(o,par)))) = getNextId
				>>= \i -> upd (\ps -> ps ++ [np i]) proposals
				>>| assignToMany (fillProposal (np i)) par
				>>* [OnValue (always (o @: editProposal (np i)))] 
			where
				starts = map (\dt -> (dt, [])) ss
				np i =  { pid = i, ptitle = t, pstarts = starts, pduration = d, powner = o, pparticipants = par}

editProposal :: Proposal -> Task [Proposal]
editProposal p = fetchFromShared proposals p
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
				-&&- viewInformation "Participants" [ViewAs (map toString)] p.pparticipants
				>>* [OnAction (Action "Create Appointment") (hasValue turnIntoApp),
					 OnAction (Action "Cancel Proposal") (always (deleteProposal p.pid))]
			where
				turnIntoApp (i,(t,(s,(d,(o,par)))))
					# na = { aid = 0, title = t, start = fst s, duration = d, owner = o, participants = par}
					 = createAppointment na 
					   >>* [OnValue (always (deleteProposal i))]
				deleteProposal :: Int -> Task [Proposal]
				deleteProposal i = upd (removeFromList (\p -> i == p.pid)) proposals

fillProposal :: Proposal -> Task [Proposal]
fillProposal p = get currentUser
		>>= \u -> viewInformation "Title" [] p.ptitle
		||- (enterMultipleChoice "Choose available start times" [ChooseFromCheckGroup id] (map fst p.pstarts)
		-|| viewInformation "Duration" [] p.pduration
		-|| viewInformation "Owner" [] (toString p.powner)
		-|| viewInformation "Participants" [] (map toString p.pparticipants))
		>>* [OnAction ActionOk (hasValue (updateProposal u p))]
	where
		updateProposal :: User Proposal [DateTime] -> Task [Proposal]
		updateProposal u p sts = upd (\ps -> map (updateStarts u p sts) ps) proposals 
			where 
				updateStarts :: User Proposal [DateTime] Proposal -> Proposal
				updateStarts u np sts op 
					| np == op = {op & pstarts = addStarts op.pstarts sts u}
					| otherwise = op
					where
						addStarts :: [(DateTime, [User])] [DateTime] User -> [(DateTime, [User])]
						addStarts sts ts u = [let users = if (d1 == d2) (addUnique u us) us in (d2, users) \\ (d2,us) <- sts, d1 <- ts]


