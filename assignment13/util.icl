implementation module util

import util

:: MyMap a b :== [(a,b)]

getM :: k (MyMap k v) -> Maybe v | == k
getM _ [] = Nothing
getM x [(k,v):ys] = if (x == k) (Just v) (getM x ys)

putM :: k v (MyMap k v) -> MyMap k v | == k
putM k v [] = [(k,v)]
putM k v [(a,b):ys] = if (k == a) ([(k,v):ys]) (putM k v ys)

