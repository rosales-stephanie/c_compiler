module Lexer 
(
    lexer,
    lexer',
    validTokens,
) where

import Data.Char
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Tokens


lexer :: String -> [Token]
lexer output = 
    let eitherTokens = foldl(\acc x -> createToken x : acc) [] (seperateTokens $ output)
        tokens = foldl(\acc x -> case x of
                                    Left _ -> acc
                                    Right tok -> tok : acc) [] eitherTokens
    in tokens 


validTokens :: String -> Bool
validTokens s = 
    let eitherTokens = lexer' s
    in foldl(\acc x -> case x of 
                            Left _ -> False
                            Right _ -> acc) True eitherTokens


lexer' :: String -> [Either String Token]
lexer' output = reverse $ 
    (foldl(\acc x -> createToken x : acc) [] (seperateTokens $ output))


createToken :: String -> Either String Token
createToken s
    | s == "int"                       = Right Tokens.KeywordInt
    | s == "void"                      = Right Tokens.KeywordVoid
    | s == "return"                    = Right Tokens.KeywordReturn
    | foldl(\acc x -> if not . isDigit $ x 
                      then False 
                      else acc) True s = Right $ Tokens.Constant (read s)
    | s == "("                         = Right Tokens.OpenParen
    | s == ")"                         = Right Tokens.CloseParen
    | s == "{"                         = Right Tokens.OpenBracket
    | s == "}"                         = Right Tokens.CloseBracket
    | s == ";"                         = Right Tokens.Semicolon
    | (not . isDigit . head $ s) && 
    foldl(\acc x -> if (not . isAlphaNum $ x) || (x == '_')
                      then False 
                      else acc) True s = Right $ Tokens.Identifier s
    | otherwise                        = Left $ "Invalid Token: " ++ s
--How to use guards in foldl?


seperateTokens :: String -> [String]
seperateTokens = 
    words . reverse . foldl(\acc x -> 
    if x == '(' || x == ')' || x == '{' || x == '}' || x == ';' 
    then ' ' : x : ' ' : acc 
    else x:acc ) ""
