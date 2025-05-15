module CodeGen 
(
    gen
) where

import qualified Ast 
import Assembly

convertExp :: Ast.Constant -> Operand
convertExp (Ast.Constant num) = Imm num


convertStatement :: Ast.Return -> [Instruction]
convertStatement (Ast.Return c) = 
    let imm = convertExp c
    in [Mov imm Register, Ret]


convertFunc :: Ast.FuncDef -> FuncDef
convertFunc f = 
    let ins = convertStatement $ Ast.body f
    in FuncDef {name=(Ast.name f), instructions=ins}


gen :: Ast.Program -> Program
gen (Ast.Program f) = 
    let func = convertFunc f
    in Program func



