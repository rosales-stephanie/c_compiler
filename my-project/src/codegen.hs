module CodeGen 
(
    gen
) where

import qualified Ast 
import Assembly

convertExp :: Ast.Exp -> Operand
convertExp (Ast.Exp (Ast.Constant num)) = Imm num


convertStatement :: Ast.Statement -> [Instruction]
convertStatement (Ast.Statement (Ast.Return exp)) = 
    let imm = convertExp exp
    in [Mov imm Register, Ret]


convertFunc :: Ast.FuncDef -> FuncDef
convertFunc f = 
    let ins = convertStatement $ Ast.body f
    in FuncDef {name=(Ast.name f), instructions=ins}


gen :: Ast.Program -> Program
gen (Ast.Program f) = 
    let func = convertFunc f
    in Program func



