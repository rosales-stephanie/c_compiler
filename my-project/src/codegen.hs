module CodeGen 
(
    tackyToAssembly
) where

import qualified Tacky as T
import Assembly
import qualified Data.Map as Map


--assembly -> assembly
--returns whether the num was used or not, as well as the operand
pseudoToStack'' :: Operand -> Map.Map String Int -> Int -> (Map.Map String Int, Operand, Int)
pseudoToStack'' op map num = 
    case op of 
        Pseudo s -> 
            let maybeNum = Map.lookup s map
            in case maybeNum of
                Just n  -> (map, Stack n, num)
                Nothing -> (Map.insert s num map, Stack num, num - 4)
        _        -> (map, op, num)


--assembly -> assembly
pseudoToStack :: [Instruction] -> [Instruction]
pseudoToStack ins = 
    let map      = Map.empty
        stackNum = -4
    in pseudoToStack' ins map stackNum

    
--assembly -> assembly
pseudoToStack' :: [Instruction] -> Map.Map String Int -> Int -> [Instruction]
pseudoToStack' [] _ _ = []
pseudoToStack' (x : xs) map num = 
    case x of 
        Mov src dst      -> 
            let (m1, opS, n1) = pseudoToStack'' src map num
                (m2, opD, n2) = pseudoToStack'' dst m1 n1
            in (Mov opS opD) : (pseudoToStack' xs m2 n2)
        Ins (Unary uOp op) -> 
            let (m1, nOp, n1) = pseudoToStack'' op map num
            in (Ins (Unary uOp nOp)) : (pseudoToStack' xs m1 n1)
        _           -> x : (pseudoToStack' xs map num)


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
        nIn = pseudoToStack ins
    in FuncDef {name=(T.name func), ins=nIn}


tackyToAssembly :: T.Program -> Program
tackyToAssembly (T.Program func) = 
    let assemblyFunc = convertFunc func
    in Program assemblyFunc
