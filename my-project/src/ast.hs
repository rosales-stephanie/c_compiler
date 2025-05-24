module Ast
( 
  UnaryOp(..),
  Exp(..),
  Statement(..),
  FuncDef(..),
  Program(..)
) where

data UnaryOp   = Complement | Negate
data Exp       = Constant Int | Unary UnaryOp Exp 
data Statement = Return Exp
data FuncDef   = FuncDef { name :: String, body :: Statement}
data Program   = Program FuncDef

instance Show UnaryOp where
    show Complement = "complement"
    show Negate     = "negate"

instance Show Exp where
    show (Constant n) = "Exp(" ++ show n ++ ")"
    show (Unary op exp) = "Unary(" ++ show op ++ show exp ++ ")"

instance Show Statement where
    show (Return e) = "Statement(\n\t" ++ show e ++ "\n\t)"

instance Show FuncDef where
    show (FuncDef name body) = "Function(\n\tname:\
                     \ " ++ name ++ ",\n\tbody:\
                     \ " ++ show body ++ "\n    )"

instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)"
