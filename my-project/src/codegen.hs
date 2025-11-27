module CodeGen 
(
    tackyToAssembly
) where

import qualified Tacky as T
import Assembly
import qualified Data.Map as Map


--fix invalid Mov instructions where both the source and destination are
--Stack operands
fixMovIns :: [Instruction] -> [Instruction]
fixMovIns [] = []
fixMovIns (x : xs) = 
   case x of
        Mov (Stack n1) (Stack n2) -> 
            (Mov (Stack n1) (Reggie R10)) : (Mov (Reggie R10) (Stack n2)) : (fixMovIns xs)
        _ -> x : (fixMovIns xs)


--assembly -> assembly
pseudoToStack'' :: Operand -> Map.Map String Int -> Int -> (Map.Map String Int, Operand, Int)
pseudoToStack'' op map num = 
    case op of 
        Pseudo s -> 
            let maybeNum = Map.lookup s map
            in case maybeNum of
                Just n  -> (map, Stack n, num)
                Nothing -> (Map.insert s (num - 4) map, Stack (num - 4), num - 4)
        _        -> (map, op, num)


--assembly -> assembly
pseudoToStack :: [Instruction] -> ([Instruction], Int)
pseudoToStack ins = 
    let map      = Map.empty
        stackNum = 0
    in pseudoToStack' ins map stackNum

    
--assembly -> assembly
pseudoToStack' :: [Instruction] -> Map.Map String Int -> Int -> ([Instruction], Int)
pseudoToStack' [] _ n = ([], n)
pseudoToStack' (x : xs) map num = 
    case x of 
        Mov src dst        -> 
            let (m1, opS, n1)  = pseudoToStack'' src map num
                (m2, opD, n2)  = pseudoToStack'' dst m1 n1
                (ins, sOffset) = pseudoToStack' xs m2 n2
            in ((Mov opS opD) : ins, sOffset) 
        Ins (Unary uOp op) -> 
            let (m1, nOp, n1)  = pseudoToStack'' op map num
                (ins, sOffset) = pseudoToStack' xs m1 n1
            in ((Ins (Unary uOp nOp)) : ins, sOffset)
        _                  -> 
            let (ins, sOffset) = pseudoToStack' xs map num
            in (x : ins, sOffset)


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


tackyToAssemblyUnary :: T.UnaryOp -> T.Val -> T.Val -> (Instruction, Instruction)
tackyToAssemblyUnary op src dest = 
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
        T.Unary op src dst -> 
            let (am, au) = tackyToAssemblyUnary op src dst
            in am : au : (tackyToAssemblyIns xs)


convertFunc :: T.FuncDef -> FuncDef
convertFunc func = 
    let ins            = tackyToAssemblyIns $ T.ins func
        (nIn, sOffset) = pseudoToStack ins
        addAllIn       = (AllocateStack sOffset) : nIn
        fixedMov       = fixMovIns addAllIn
    in FuncDef {name=(T.name func), ins=fixedMov}


tackyToAssembly :: T.Program -> Program
tackyToAssembly (T.Program func) = 
    let assemblyFunc = convertFunc func
    in Program assemblyFunc
