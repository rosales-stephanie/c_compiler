module Tacky 
(
    UnaryOp(..),
    Unary(..),
    Instruction(..),
    Val(..),
    FuncDef(..),
    Program(..)
) where

data UnaryOp = Complement | Negate deriving (Show)
data Unary = Unary {op :: UnaryOp, src :: Val, dest :: Val}
data Instruction = Return Val | Ins Unary
data Val = Constant Int | Var String deriving (Show)
data FuncDef = FuncDef {name :: String, ins :: [Instruction]}
data Program = Program FuncDef

instance Show Instruction where 
    show (Return v) = "Return " ++ show v
    show (Ins u) = show u

instance Show Unary where
    show (Unary op src dst) = "Unary(\n\t    op:\
                     \ " ++ show op ++ ",\n\t    src:\
                     \ " ++ show src ++ ",\n\t    dst:\
                     \ " ++ show dst ++ ")"

instance Show FuncDef where
    show (FuncDef name ins) = "Function(\n\tname:\
                     \ " ++ show name ++ ",\n\tbody:\
                     \ \n\t[" ++ (foldl (\acc x -> acc ++ "\n\t  " ++ show x) "" ins) ++ "\n\t]\n    )"

instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)\n"
