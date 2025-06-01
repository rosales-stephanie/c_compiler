module CodeGen 
(
    gen
) where

import qualified Tacky as T
import Assembly


genIns :: [T.Instruction] -> [Instruction]
genIns ins = [Ret]



convertFunc :: T.FuncDef -> FuncDef
convertFunc func = 
    let ins = genIns $ T.ins func
    in FuncDef {name=(T.name func), ins=ins}


gen :: T.Program -> Program
gen (T.Program func) = 
    let assemblyFunc = convertFunc func
    in Program assemblyFunc
