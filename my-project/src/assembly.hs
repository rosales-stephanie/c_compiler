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
    show (Imm num) = "Imm " ++ show num
    show Register = "Register"
instance Show Instruction where
    show (Mov op1 op2) = "Mov (" ++ show op1 ++ ", " ++ show op2 ++ ")"
    show Ret = "Ret"
instance Show FuncDef where
    show (FuncDef name body) = "Function(\n\tname:\
                              \ " ++ name ++ ",\n\tbody:\
                              \ " ++ show body ++ "\n    )"
instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)"
