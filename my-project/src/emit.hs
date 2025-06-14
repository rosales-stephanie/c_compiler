module Emit 
(
    emitProg
) where

import Text.Regex.PCRE
import Assembly

emitProg :: String -> Program -> IO()
emitProg filename (Program ast) =
    let assembly = emitFunc ast
        pattern  = "[^.]+"
    in writeFile ((filename =~ pattern :: String) ++ ".s") assembly


emitOperand :: Operand -> String
emitOperand op = 
    case op of 
        Imm num    -> "$" ++ show num
        Reggie AX  -> "%eax"
        Reggie R10 -> "%r10d"
        Stack num  -> show num ++ "(%rbp)"



emitIns :: [Instruction] -> [String]
emitIns [] = []
emitIns (x : xs) = 
    case x of
        Mov op1 op2                       -> 
            let a1 = emitOperand op1
                a2 = emitOperand op2
            in ("movl " ++ a1 ++ ", " ++ a2) : emitIns xs
        Ret                               -> 
            "movq %rbp, %rsp" : "popq %rbp" : "ret" : emitIns xs
        Ins (Unary {unaryOp=Neg, op=op'}) ->
            ("negl " ++ (emitOperand op')) : emitIns xs
        Ins (Unary {unaryOp=Not, op=op'}) ->
            ("notl " ++ (emitOperand op')) : emitIns xs
        AllocateStack num -> ("subq $" ++ show num ++ ", %rsp") : emitIns xs



emitFunc :: FuncDef -> String
emitFunc (FuncDef {name=name', ins=ins'}) = 
    let arrIns = emitIns ins'
        sIns   = foldl (\acc x -> ("    " ++ x) : acc) [] arrIns
        revIns = reverse sIns
        res    = unlines revIns
    in "    .global _" ++ name' ++ "\n\
        \_" ++ name' ++ ":\n\
        \    pushq %rbp\n\
        \    movq %rsp, %rbp\n" ++ res
