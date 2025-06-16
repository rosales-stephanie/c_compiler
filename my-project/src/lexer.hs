module Lexer 
(
    lexer,
    validTokens,
    errors
) where

import Data.Char
import Data.List (group)
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Tokens
import Text.Regex.PCRE


lexer :: String -> [Token]
lexer s = 
    let eitherToks = eitherTokens s
        tokens = reverse $ foldl(\acc x -> case x of
                                    Left _ -> acc
                                    Right tok -> tok : acc) [] eitherToks
    in tokens 


validTokens :: String -> Bool
validTokens s = 
    let eitherToks = eitherTokens s
    in foldl(\acc x -> case x of 
                            Left _ -> False
                            Right _ -> acc) True eitherToks


eitherTokens :: String -> [Either String Token]
eitherTokens s = reverse $ foldl(\acc x -> createToken x : acc) [] (seperateTokens $ s)


errors :: String -> [String]
errors output = 
    let eitherToks = eitherTokens output
        tokens = foldl(\acc x -> case x of
                                    Left s -> s : acc
                                    _ -> acc) [] eitherToks
    in tokens 


createToken :: String -> Either String Token
createToken s = 
    let matchNums = "^[0-9]+$"
        matchId   = "^[a-zA-Z_][0-9a-zA-Z_]*$"
    in case () of 
        _ | s == "int"               -> Right KeywordInt
          | s == "void"              -> Right KeywordVoid
          | s == "return"            -> Right KeywordReturn
          | (s =~ matchNums :: Bool) -> Right $ Constant (read s :: Int)
          | s == "("                 -> Right OpenParen
          | s == ")"                 -> Right CloseParen
          | s == "{"                 -> Right OpenBracket
          | s == "}"                 -> Right CloseBracket
          | s == ";"                 -> Right Semicolon
          | (s =~ matchId :: Bool)   -> Right $ Identifier s
          | (s =~ "--" :: Bool)      -> Right DecrementOp
          | s == "-"                 -> Right Hyphen
          | s == "~"                 -> Right Tilde
          | s == "+"                 -> Right Plus
          | s == "*"                 -> Right Asterisk
          | s == "/"                 -> Right ForwardSlash
          | s == "%"                 -> Right Percent
          | otherwise                -> Left $ "Invalid Token: " ++ s


addSpaces :: String -> String
addSpaces s = foldl(\acc x -> if x == "--" 
                            then (acc ++ " -- ")
                            else (acc ++ x)) "" (group s)


seperateTokens :: String -> [String]
seperateTokens s = 
    words . reverse $ foldl(\acc x -> 
    if x == '(' || x == ')' || x == '{' || x == '}' || x == ';' || x == '~' 
    || x == '-' || x == '+' || x == '*' || x == '/' || x == '%'
    then ' ' : x : ' ' : acc 
    else x:acc ) "" (addSpaces s)
