definition module util

import Data.Maybe

:: MyMap a b :== [(a,b)]

getM :: k (MyMap k v) -> Maybe v | == k
putM :: k v (MyMap k v) -> MyMap k v | == k