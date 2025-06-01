module GenTacky 
(
    emitTackyProg
) where

import qualified Ast
import Tacky

emitTackyIn' :: Ast.Exp -> String -> Int -> (Val, [Instruction])
emitTackyIn' exp funcName num = 
    case exp of
        Ast.Constant n -> (Constant n, [])
        Ast.Unary op inner -> 
            let (src, innerIns) = emitTackyIn' inner funcName counter
                counter = num + 1
                destName = funcName ++ "." ++ (show counter) 
                dest = Var destName
            in case op of
                Ast.Complement ->
                    let retIns = (Ins Unary {op = Complement, src = src, dest = dest}) : innerIns
                    in (dest, retIns)
                Ast.Negate ->
                    let retIns = (Ins Unary {op = Negate, src = src, dest = dest}) : innerIns
                    in (dest, retIns)


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
