definition module AUser

import iTasks

:: UserID :== String
:: AUser = Root UserID | User UserID

derive class iTask AUser