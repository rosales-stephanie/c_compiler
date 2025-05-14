module Ast
( 
  Constant(..),
  Return(..),
  FuncDef(..),
  Program(..)
) where

data Constant = Constant Int 
data Return   = Return Constant
data FuncDef  = FuncDef { name :: String, body :: Return}
data Program  = Program FuncDef

instance Show Constant where
    show (Constant n) = "Constant(" ++ show n ++ ")"
instance Show Return where
    show (Return c) = "Return(\n\t" ++ show c ++ "\n\t)"
instance Show FuncDef where
    show (FuncDef name body) = "Function(\n\tname:\
                     \ " ++ name ++ ",\n\tbody:\
                     \ " ++ show body ++ "\n    )"
instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)"
