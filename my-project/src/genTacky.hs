module GenTacky 
(
    emitTackyProg
) where

import qualified Ast
import Tacky

emitTackyOp :: Ast.BinaryOp -> BinaryOp
emitTackyOp operand = 
    case operand of
        Ast.Add        -> 
        Ast.Subtract   -> 
        Ast.Multiply   -> 
        Ast.Divide     -> 
        Ast.Remainder  -> 
        Ast.AND        ->
        Ast.OR         ->
        Ast.XOR        -> 
        Ast.LeftShift  -> 
        Ast.RightShift ->


emitTackyIn' :: Ast.Exp -> String -> Int -> (Val, [Instruction])
emitTackyIn' exp funcName num = 
    case exp of
        Ast.Constant n     -> (Constant n, [])
        Ast.Binary op exp1 exp2 -> 
            let (src1, innerIns1) = emitTackyIn' exp1 funcName counter
                (src2, innerIns2) = emitTackyIn' exp1 funcName counter
                counter = num + 1
                destName = funcName ++ "." ++ (show counter) 
                d = Var destName -- type Tacky.Val
            in case op of
                Ast.Complement -> 
                    let retIns = (Ins Unary {
                        op = Complement, src = s, dest = d}) : innerIns
                    in (d, retIns)
                Ast.Negate -> 
                    let retIns = (Ins Unary {
                        op = Negate, src = s, dest = d}) : innerIns
                    in (d, retIns)
        Ast.Unary op inner -> 
            let (s, innerIns) = emitTackyIn' inner funcName counter
                counter = num + 1
                destName = funcName ++ "." ++ (show counter) 
                d = Var destName -- type Tacky.Val
            in case op of
                Ast.Complement -> 
                    let retIns = (Ins Unary {
                        op = Complement, src = s, dest = d}) : innerIns
                    in (d, retIns)
                Ast.Negate -> 
                    let retIns = (Ins Unary {
                        op = Negate, src = s, dest = d}) : innerIns
                    in (d, retIns)


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
