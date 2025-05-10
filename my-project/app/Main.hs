module Main (main) where

import System.Environment
import System.Exit
import Data.List
import Text.Regex.PCRE
import Tokens
import Lexer


main = do
   args <- getArgs
   contents <- getContents
   if length args > 0 -- lex, parse, codegen
       then if validTokens contents --step 1: lex
                then if "lex" == (args !! 0)
                         then do exitWith ExitSuccess
                     else 
                         --lex
                         --parse
                         if "parse" == (args !! 0)
                             then do putStrLn "parse"
                         else do
                            --codegen
                            putStrLn "codegen"
            else do
                putStrLn "Error: lexer failed"
                print $ lexer' contents
                exitWith (ExitFailure 2)
   else putStrLn "no args"
   putStrLn "parser.hs"
