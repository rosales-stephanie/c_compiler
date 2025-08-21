module Main (main) where

import System.Environment
import System.IO
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
    (stage : filename : _) <- getArgs
    contents <- readFile filename
    if validTokens contents then --check if valid tokens
        if stage == "--lex" then do 
            print $ lexer contents
            exitWith ExitSuccess
        else 
            let tokens = lexer contents --lex tokens
                parsedToks = parseProgram tokens --parse tokens
            in case parsedToks of
                Left err  -> 
                    do
                    putStr err
                    exitWith $ ExitFailure 2
                Right ast -> do
                    if stage == "--parse" then do 
                        print ast
                        exitWith ExitSuccess
                    else
                        let tackyStruct = emitTackyProg ast
                            assemblyT   = tackyToAssembly tackyStruct
                        in case () of
                        _ | stage == "--codegen" ->
                            --assembly generation but stop before code emission
                                do putStrLn . show $ assemblyT
                          | stage == "--tacky"   ->
                                do putStr . show $ tackyStruct
                          | otherwise            -> 
                                --S or nothing
                                --output an assembly file with a .s extension
                                --but do not assemble or link it
                                do emitProg filename assemblyT    
    else do
        putStrLn "Error: lexer failed"
        print $ Lexer.errors contents
        exitWith $ ExitFailure 2
