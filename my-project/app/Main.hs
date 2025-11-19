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
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

-- Using StateT over Writer monad, wrapped with MaybeT to handle Nothing
type Parser a = MaybeT (StateT [Token] (Writer String)) a

runParser :: Parser a -> [Token] -> ((Maybe a, [Token]), String)
runParser p initTokens = runWriter $ runStateT (runMaybeT p) initTokens

main = do 
    (stage : filename : _) <- getArgs
    contents <- readFile filename
    let toks = lexer contents
    case toks of 
        Right tokens ->
            if stage == "--lex" then do 
                print $ lexer contents
                exitWith ExitSuccess
            else 
                let ((parsedToks, leftovers), log) = runParser parseProgram tokens
                in case parsedToks of
                    Nothing -> do
                        putStr log
                        print leftovers
                        exitWith $ ExitFailure 2
                    Just ast -> do
                        if stage == "--parse" then do 
                            print ast
                            exitWith ExitSuccess
                        else
                            let tackyStruct = emitTackyProg ast
                                assemblyT   = tackyToAssembly tackyStruct
                            in case () of
                                _ | stage == "--codegen" ->
                                        -- assembly generation 
                                        -- but stop before code emission
                                        do putStrLn . show $ assemblyT
                                  | stage == "--tacky" ->
                                        do putStr . show $ tackyStruct
                                  | otherwise -> 
                                        -- S or nothing
                                        -- output an assembly file 
                                        -- with a .s extension
                                        -- but do not assemble or link it
                                        do emitProg filename assemblyT    
        Left s -> do
            putStrLn "Error: lexer failed"
            putStr s
            exitWith $ ExitFailure 2
