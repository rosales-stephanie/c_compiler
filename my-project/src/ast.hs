module Ast
( 
  UnaryOp(..),
  BinaryOp(..),
  Exp(..),
  Statement(..),
  FuncDef(..),
  Program(..)
) where

data BinaryOp  = Add | Subtract | Multiply | Divide | Remainder deriving (Show)
data UnaryOp   = Complement | Negate deriving (Show)
data Exp       = Constant Int | Unary UnaryOp Exp | Binary BinaryOp Exp Exp
data Statement = Return Exp
data FuncDef   = FuncDef {name :: String, body :: Statement}
data Program   = Program FuncDef

instance Eq BinaryOp where
    Add == Subtract       = True
    Add == _              = False
    Subtract == _         = False
    Multiply == Add       = False
    Multiply == Subtract  = False
    Divide == Add         = False
    Divide == Subtract    = False
    Remainder == Add      = False
    Remainder == Subtract = False
    _ == _                = True

instance Ord BinaryOp where
    compare Subtract Multiply = LT
    compare Subtract Divide = LT
    compare Subtract Remainder = LT
    compare Add Multiply = LT
    compare Add Divide = LT
    compare Add Remainder = LT
    compare Multiply Subtract  = GT
    compare Divide Subtract  = GT
    compare Remainder Subtract  = GT
    compare Multiply Add = GT
    compare Divide Add = GT
    compare Remainder Add = GT
    compare _ _ = EQ

instance Show Exp where
    show (Constant n) = "Constant(" ++ show n ++ ")"
    show (Unary op exp) = "Unary(" ++ show op ++ ",\n\t    " ++ show exp ++ ")"
    show (Binary binOp left right) = "Binary(" ++ show binOp ++ "\
                                        \, " ++ show left ++ "\
                                        \, " ++ show right ++ ")"

instance Show Statement where
    show (Return e) = "Statement(\n\t" ++ show e ++ "\n\t)"

instance Show FuncDef where
    show (FuncDef name body) = "Function(\n\tname:\
                     \ " ++ name ++ ",\n\tbody:\
                     \\n\t" ++ show body ++ "\n    )"

instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)"
