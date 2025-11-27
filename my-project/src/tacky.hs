module Tacky 
(
    UnaryOp(..),
    BinaryOp(..),
    Instruction(..),
    Val(..),
    FuncDef(..),
    Program(..)
) where

data BinaryOp = 
    Add 
    | Subtract 
    | Multiply 
    | Divide 
    | Remainder 
    | AND 
    | XOR 
    | OR 
    | LeftShift
    | RightShift deriving (Show)

data UnaryOp = Complement | Negate deriving (Show)
data Val = Constant Int | Var String deriving (Show)

data Instruction = 
    Return Val 
    | Unary {op :: UnaryOp, src :: Val, dest :: Val}
    | Binary {binOp :: BinaryOp, src1 :: Val, src2 :: Val, bDest :: Val}

data FuncDef = FuncDef {name :: String, ins :: [Instruction]}
data Program = Program FuncDef

instance Show Instruction where 
    show (Return v) = "Return " ++ show v
    show (Binary op v1 v2 dst) = "Binary(\n\t    op:\
                     \ " ++ show op ++ ",\n\t    src1:\
                     \ " ++ show v1 ++ ",\n\t    src2:\
                     \ " ++ show v2 ++ ",\n\t    dst:\
                     \ " ++ show dst ++ ")"
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
