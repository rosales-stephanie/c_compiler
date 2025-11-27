module GenTacky 
(
    emitTackyProg
) where

import qualified Ast
import Tacky

emitTackyOp :: Ast.BinaryOp -> BinaryOp
emitTackyOp operand = 
    case operand of
        Ast.Add        -> Add
        Ast.Subtract   -> Subtract
        Ast.Multiply   -> Multiply
        Ast.Divide     -> Divide
        Ast.Remainder  -> Remainder
        Ast.AND        -> AND
        Ast.OR         -> OR
        Ast.XOR        -> XOR
        Ast.LeftShift  -> LeftShift
        Ast.RightShift -> RightShift


emitTackyIn' :: Ast.Exp -> String -> Int -> (Val, [Instruction])
emitTackyIn' exp funcName num = 
    case exp of
        Ast.Constant n     -> (Constant n, [])
        Ast.Binary op exp1 exp2 -> 
            let (src1, innerIns1) = emitTackyIn' exp1 funcName counter
                (src2, innerIns2) = emitTackyIn' exp1 funcName counter
                combinedIns       = innerIns1 ++ innerIns2
                counter           = num + 1
                destName          = funcName ++ "." ++ (show counter) 
                finalDest         = Var destName -- type Tacky.Val
                operand           = emitTackyOp op
                finalBinOp        = Binary operand src1 src2 finalDest 
            in (finalDest, finalBinOp : combinedIns)
        Ast.Unary op inner -> 
            let (s, innerIns) = emitTackyIn' inner funcName counter
                counter = num + 1
                destName = funcName ++ "." ++ (show counter) 
                finalDest = Var destName -- type Tacky.Val
            in case op of
                Ast.Complement -> 
                    let retIns = (Unary {
                        op = Complement, src = s, dest = finalDest}) : innerIns
                    in (finalDest, retIns)
                Ast.Negate -> 
                    let retIns = (Unary {
                        op = Negate, src = s, dest = finalDest}) : innerIns
                    in (finalDest, retIns)


emitTackyIn :: Ast.Statement -> String -> Int -> [Instruction]
emitTackyIn (Ast.Return exp) funcName counter = 
    let (dest, ins) = emitTackyIn' exp funcName counter
    in reverse $ (Return dest) : ins


emitTackyFunc :: Ast.FuncDef -> FuncDef
emitTackyFunc Ast.FuncDef {Ast.name=n, Ast.body=b} = 
    let ins = emitTackyIn b n 0
    in FuncDef {name=n, ins=ins}


emitTackyProg :: Ast.Program -> Program
emitTackyProg (Ast.Program func) = 
    let tackyFunc = emitTackyFunc func
    in Program tackyFunc
