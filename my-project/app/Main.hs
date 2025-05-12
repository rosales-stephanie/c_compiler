module Main (main) where

import System.Environment
import System.Exit
import Data.List
import Tokens
import Lexer
import Parser


main = do
    args <- getArgs
    contents <- getContents
    if validTokens contents --check if valid tokens
        then if length args > 0 && "lex" == (args !! 0)
                 then do exitWith ExitSuccess
             else 
                 --parse
                 let tokens = lexer contents --lex tokens
                     validP = validParse tokens --check if valid parse
                 in if not validP 
                        then do 
                        print $ Parser.errors tokens
                        putStrLn "Error: parser failed"
                        exitWith $ ExitFailure 2
                    else 
                        if length args > 0 && "parse" == (args !! 0)
                            then do 
                            exitWith ExitSuccess
                        else if length args > 0 && "codegen" == (args !! 0)
                            --codegen 
                            --assembly generation but stop before code emission
                            then do putStrLn "codegen"
                        else if length args > 0 && "S" == (args !! 0)
                            --emit an assembly file but do not assemble or link it
                            then do putStrLn "S"
                        else do
                            --output an assembly file with a .s extension
                            --assemble and link the file to produce an 
                            --executable and then delete the assembly file
                            putStrLn "fin"
    else do
        putStrLn "Error: lexer failed"
        print $ Lexer.errors contents
        exitWith $ ExitFailure 2
