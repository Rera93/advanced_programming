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
showProposals = viewSharedInformation ("Open proposals", "Choose a proposal to view") [] proposals

makeProposal :: Task [Proposal]
makeProposal = get currentUser
		>>= \u -> forever $ getNextId
		-&&- enterInformation "Title" []
		-&&- updateInformation "Suggested Start Times" [] []
		-&&- updateInformation "Duration" [] defaultDuration
		-&&- viewInformation "Owner" [ViewAs toString] u
		-&&- selectUsers
		>>* [OnAction (Action "Create") (hasValue createProposal),
			 OnAction ActionCancel (always (return defaultValue))] // TODO: Check what to do here
	where 
		createProposal :: (Int,(String, ([DateTime], (Time, (User, [User]))))) -> Task [Proposal]
		createProposal (i,(t,(ss,(d,(o,par)))))
			# starts = map (\dt -> (dt, [])) ss
			# np = { pid = i, ptitle = t, pstarts = starts, pduration = d, powner = o, pparticipants = par}
			= upd (\ps -> ps ++ [np]) proposals
				>>| assignToMany (fillProposal (np)) par
				>>* [OnValue (always (o @: editProposal (np)))] 

editProposal :: Proposal -> Task [Proposal]
editProposal p = viewInformation "Edit Proposal" [] []

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


