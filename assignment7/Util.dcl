// Matheus Amazonas Cabral de Andrade
// s4605640

definition module Util

import iTasks
from iTasks.Extensions.DateTime import :: Time

selectUsers :: Task [User]
assignToMany :: (Task a) [User] -> Task [a] | iTask a
defaultDuration :: Time
addUnique :: a [a] -> [a] | gEq{|*|} a
getNextId :: Task Int
removeFromList :: (a -> Bool) [a] -> [a]