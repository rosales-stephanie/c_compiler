module Emit 
(
    emit
) where

import Assembly

emit :: String -> Program -> IO()
emit filename ast =
    let assembly = show ast
    in writeFile filename assembly
