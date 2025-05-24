module Tacky 
(
    UnaryOp(..),
    Instruction(..),
    Val(..),
    FuncDef(..),
    Program(..)
) where

data Identifier = Identifier String
data UnaryOp = Complement | Negate
data Unary = Unary {op :: UnaryOp, src :: Val, dest :: Val}
data Instruction = Return Val | Instruction Unary
data Val = Constant Int | Var Identifier
data FuncDef = FuncDef {name :: Identifier, instructions :: [Instruction]}
data Program = Program FuncDef
