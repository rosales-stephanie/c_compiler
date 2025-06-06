module Main (main) where

import System.Environment
import System.Exit
import Tokens
import Lexer
import Parser
import Ast
import Assembly 
import CodeGen
import Emit
import Tacky
import GenTacky

main = do
    args <- getArgs
    contents <- getContents
    if validTokens contents then --check if valid tokens
        if length args > 0 && "lex" == (args !! 0) then do 
            print $ lexer contents
            exitWith ExitSuccess
        else 
            --parse
            let tokens = lexer contents --lex tokens
                parsedToks = parseProgram tokens
            in case parsedToks of
                Left err -> 
                    do
                    putStr err
                    exitWith $ ExitFailure 2
                Right ast -> do
                    if length args > 0 && "parse" == (args !! 0) then do 
                        exitWith ExitSuccess
                    else
                        let tackyStruct = emitTackyProg ast
                            assemblyT      = tackyToAssembly tackyStruct
                        in case () of
                        _ | length args > 0 && "codegen" == (args !! 0) ->
                            --assembly generation but stop before code emission
                                do putStrLn . show $ assemblyT
                          | length args > 0 && "tacky" == (args !! 0) ->
                                do putStr . show $ tackyStruct
                          | otherwise ->
                                --S
                                -- (args !! 0) filename
                                do emit (args !! 0) assemblyT    
    else do
        putStrLn "Error: lexer failed"
        print $ Lexer.errors contents
        exitWith $ ExitFailure 2
