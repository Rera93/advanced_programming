definition module question4

// ---------- 4a ----------

class gram v where
	lit :: String -> v
	idn :: v 
	int :: v 
	sequ :: [v] -> v
	alt :: [v] -> v
	def :: (v -> (v, v)) -> v