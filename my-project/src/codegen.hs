module CodeGen 
(
    gen
) where

import qualified Ast 
import Assembly

convertExp :: Ast.Exp -> Operand
convertExp (Ast.Constant num) = Imm num


convertStatement :: Ast.Statement -> [Instruction]
convertStatement (Ast.Return exp) = 
    let imm = convertExp exp
    in [Mov imm Register, Ret]


convertFunc :: Ast.FuncDef -> FuncDef
convertFunc func = 
    let ins = convertStatement $ Ast.body func
    in FuncDef {name=(Ast.name func), instructions=ins}


gen :: Ast.Program -> Program
gen (Ast.Program func) = 
    let assemblyFunc = convertFunc func
    in Program assemblyFunc



