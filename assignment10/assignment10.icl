module assignment10

:: Expression = 
    New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     Set
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Logical = 
    TRUE | FALSE
  | (In) infix 4 Elem Set
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt = 
      Logical Logical
    | Expression Expression
    | If Logical Stmt Stmt
    | For Ident Set Stmt
    
:: Set    :== Expression
:: Elem  :== Expression
:: Ident  :== String

Start = 1

  

