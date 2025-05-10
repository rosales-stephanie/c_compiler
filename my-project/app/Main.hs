module Main (main) where

import System.Environment
import System.Exit
import Data.List
import Text.Regex.PCRE
import Tokens
import Lexer
import Parser


main = do
   args <- getArgs
   contents <- getContents
   if length args > 0 -- lex, parse, codegen
       then if validTokens contents --lex
                then if "lex" == (args !! 0)
                         then do exitWith ExitSuccess
                     else 
                         --parse
                         let tokens = lexer contents
                             validP = validParse tokens
                         in if not validP 
                                then do 
                                print tokens
                                print $ Parser.errors tokens
                                exitWith $ ExitFailure 2
                            else 
                                if "parse" == (args !! 0)
                                    then do exitWith ExitSuccess
                                else do
                                    --codegen
                                    putStrLn "codegen"
            else do
                putStrLn "Error: lexer failed"
                print $ Lexer.errors contents
                exitWith $ ExitFailure 2
   else do putStrLn "no args"
