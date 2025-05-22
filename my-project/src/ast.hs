module Ast
( 
  Constant(..),
  Return(..),
  UnaryOp(..),
  Exp(..),
  Statement(..),
  FuncDef(..),
  Program(..)
) where

data Constant  = Constant Int 
data UnaryOp   = Complement | Negate
data Exp       = Exp Constant | Unary UnaryOp Exp 
data Return    = Return Exp
data Statement = Statement Return 
data FuncDef   = FuncDef { name :: String, body :: Statement}
data Program   = Program FuncDef

instance Show Constant where
    show (Constant n) = "Constant(" ++ show n ++ ")"
instance Show UnaryOp where
    show Complement = "complement"
    show Negate     = "negate"
instance Show Exp where
    show (Exp c) = "Exp(" ++ show c ++ ")"
    show (Unary op exp) = "Unary(" ++ show op ++ show exp ++ ")"
instance Show Return where
    show (Return exp) = "Return(\n\t" ++ show exp ++ "\n\t)"
instance Show Statement where
    show (Statement r) = "Statement(\n\t" ++ show r ++ "\n\t)"
instance Show FuncDef where
    show (FuncDef name body) = "Function(\n\tname:\
                     \ " ++ name ++ ",\n\tbody:\
                     \ " ++ show body ++ "\n    )"
instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)"
