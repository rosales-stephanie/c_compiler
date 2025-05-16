module Assembly 
(
    Operand(..),
    Instruction(..),
    FuncDef(..),
    Program(..)
) where

data Operand = Imm Int | Register
data Instruction = Mov Operand Operand | Ret
data FuncDef = FuncDef {name :: String, instructions :: [Instruction]}
data Program = Program FuncDef

instance Show Operand where
    show (Imm num) = show num
    show Register = "%eax"

instance Show Instruction where
    show (Mov src dest) = "movq\t" ++ show src ++ ", " ++ show dest
    show Ret = "retq"

instance Show FuncDef where
    show (FuncDef name ins) = "\t.globl _" ++ name ++ "\n_" ++ name ++ ":\n" ++ (unlines $ map (\x -> "\t" ++ show x) ins)

instance Show Program where
    show (Program f) = show f
