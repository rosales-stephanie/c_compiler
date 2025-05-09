module Main (main) where

import System.Environment
import Data.List
import Text.Regex.PCRE
import Tokens
import Lexer


main = do
   args <- getArgs
   contents <- getContents
   if length args > 0 
       then putStrLn "lex"
   else putStrLn "no args"
   prettyPrintTokens contents


valid :: [Either String Token] -> Int
valid = foldl(\acc x -> case x of 
                            Left _ -> 2
                            Right _ -> acc) 0
