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
import Emit

main = do
    args <- getArgs
    contents <- getContents
    if validTokens contents then --check if valid tokens
        if length args > 0 && "lex" == (args !! 0) then do 
            exitWith ExitSuccess
        else 
            --parse
            let tokens = lexer contents --lex tokens
                parsedToks = parseProgram tokens
            in case parsedToks of
                Left err -> do
                            putStr err
                            exitWith $ ExitFailure 2
                Right ast -> do
                    if length args > 0 && "parse" == (args !! 0) then do 
                        exitWith ExitSuccess
                    else
                        let assemblyT = gen ast in 
                        if length args > 0 && "codegen" == (args !! 0) then 
                            --codegen 
                            --assembly generation but stop before code emission
                            do putStr . show $ assemblyT
                        else 
                            do emit (args !! 0) assemblyT    
    else do
        putStrLn "Error: lexer failed"
        print $ Lexer.errors contents
        exitWith $ ExitFailure 2
