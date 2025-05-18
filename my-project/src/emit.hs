module Emit 
(
    emit
) where

import Text.Regex.PCRE
import Assembly

emit :: String -> Program -> IO()
emit filename ast =
    let assembly = show ast
        pattern  = "[^.]+"
    in writeFile ((filename =~ pattern :: String) ++ ".s") assembly
