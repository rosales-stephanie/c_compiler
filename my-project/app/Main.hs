module Main (main) where

import System.Environment
import System.Exit
import Data.List
import Tokens
import Lexer
import Parser
import Ast
import Assembly 
import CodeGen

main = do
    args <- getArgs
    contents <- getContents
    if validTokens contents --check if valid tokens
        then if length args > 0 && "lex" == (args !! 0)
                 then do exitWith ExitSuccess
             else 
                 --parse
                 let tokens = lexer contents --lex tokens
                     parsedToks = parseProgram tokens
                 in case parsedToks of
                    Left err -> do
                                putStrLn err
                                exitWith $ ExitFailure 2
                    Right ast -> do
                        if length args > 0 && "parse" == (args !! 0)
                            then do 
                            exitWith ExitSuccess
                        else
                            let assemblyT = gen ast in
                                --codegen 
                                --assembly generation but stop before code emission
                                if length args > 0 && "codegen" == (args !! 0)
                                    then do print assemblyT
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
