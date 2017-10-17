definition module questionaire

:: Question = { question :: String, choices :: [String], answer :: Int}

//:: Username :== String
// I couldn't use this because the type checker complained:
//	cannot unify demanded type with offered type: Username String
//	Aren't macros supposed to run before typechecking?