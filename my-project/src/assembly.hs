module Assembly 
(
    Reg(..),
    Operand(..),
    UnaryOp(..),
    Unary(..),
    Instruction(..),
    FuncDef(..),
    Program(..)
) where

data Reg = AX | R10
data Operand = Imm Int | Register Reg | Pseudo String | Stack Int
data UnaryOp = Neg | Not deriving (Show)
data Unary = Unary {unaryOp :: UnaryOp, op :: Operand}
data Instruction = Mov Operand Operand | Ret | Ins Unary | AllocateStack Int
data FuncDef = FuncDef {name :: String, ins :: [Instruction]}
data Program = Program FuncDef

instance Show Operand where
    show (Imm num) = "$" ++ show num

instance Show Instruction where
    show (Mov src dest) = "mov\t" ++ show src ++ ", " ++ show dest
    show Ret = "ret"

instance Show FuncDef where
    show (FuncDef name ins) = "\t.globl _" ++ name ++ "\n_" ++ name ++ ":\n" ++ (unlines $ map (\x -> "\t" ++ show x) ins)

instance Show Program where
    show (Program f) = show f
