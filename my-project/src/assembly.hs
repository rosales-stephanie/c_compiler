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

data Reg = AX | R10 deriving (Show)

data Operand = Imm Int | Reggie Reg | Pseudo String | Stack Int deriving (Show)

data UnaryOp = Neg | Not deriving (Show)

data Unary = Unary {unaryOp :: UnaryOp, op :: Operand} 

data Instruction = Mov Operand Operand | Ret | Ins Unary | AllocateStack Int

data FuncDef = FuncDef {name :: String, ins :: [Instruction]}

data Program = Program FuncDef 

instance Show Instruction where
    show (Mov src dst) = "Mov " ++ show src ++ " " ++ show dst
    show Ret = "Ret"
    show (Ins u) = show u
    show (AllocateStack n) = "AllocateStack " ++ show n

instance Show Unary where
    show (Unary op dst) = "Unary(\n\t    op:\
                     \ " ++ show op ++ ",\n\t    dst:\
                     \ " ++ show dst ++ ")"

instance Show FuncDef where
    show (FuncDef name ins) = "Function(\n\tname:\
                     \ " ++ show name ++ ",\n\tbody:\
                     \ \n\t[" ++ (foldl (\acc x -> acc ++ "\n\t  " ++ show x) "" ins) ++ "\n\t]\n    )"

instance Show Program where
    show (Program f) = "Program(\n    " ++ show f ++ "\n)\n"
