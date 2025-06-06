module CodeGen 
(
    tackyToAssembly
) where

import qualified Tacky as T
import Assembly


tackyToAssemblyVal :: T.Val -> Operand
tackyToAssemblyVal v = 
    case v of 
        T.Constant n -> Imm n
        T.Var s      -> Pseudo s


tackyToAssemblyUnaryOp :: T.UnaryOp -> UnaryOp
tackyToAssemblyUnaryOp op = 
    case op of 
        T.Complement -> Not
        T.Negate     -> Neg


tackyToAssemblyUnary :: T.Unary -> (Instruction, Instruction)
tackyToAssemblyUnary (T.Unary op src dest) = 
    let s = tackyToAssemblyVal src
        d = tackyToAssemblyVal dest
        m = Mov s d
        o = tackyToAssemblyUnaryOp op
        u = Unary {unaryOp=o, op=d}
    in (Mov s d, Ins u)
        

tackyToAssemblyIns :: [T.Instruction] -> [Instruction]
tackyToAssemblyIns (x : xs) = 
    case x of
        T.Return val -> 
            let v = (tackyToAssemblyVal val)
            in (Mov v (Reggie AX)) : [Ret]
        T.Ins u -> 
            let (am, au) = tackyToAssemblyUnary u
            in am : au : (tackyToAssemblyIns xs)


convertFunc :: T.FuncDef -> FuncDef
convertFunc func = 
    let ins = tackyToAssemblyIns $ T.ins func
    in FuncDef {name=(T.name func), ins=ins}


tackyToAssembly :: T.Program -> Program
tackyToAssembly (T.Program func) = 
    let assemblyFunc = convertFunc func
    in Program assemblyFunc
