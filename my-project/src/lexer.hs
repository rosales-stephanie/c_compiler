module Lexer 
( lexer,
) where

import Data.Char
import Data.List (group)
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Tokens
import Text.Regex.PCRE


lexer :: String -> Either String [Token]
lexer s = 
    let tokens = eitherTokens s
    in if any (\x -> case x of 
                         Left err -> True
                         Right tok -> False) tokens
       then Left $ foldl(\acc x -> case x of 
                                       Left s -> acc ++ s
                                       _ -> acc) "" tokens
       else Right $ reverse $ foldl(\acc x -> case x of
                                       Right tok -> tok : acc
                                       _ -> acc) [] tokens


eitherTokens :: String -> [Either String Token]
eitherTokens s = reverse $ 
                 foldl(\acc x -> createToken x : acc) [] 
                 (addSpacesBetweenTokens $ s)


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
          | otherwise                -> Left $ "Invalid Token: " ++ s ++ "\n"


addSpacesBetweenTokens' :: String -> String
addSpacesBetweenTokens' s = foldl(\acc x -> if x == "--"
                            then acc ++ " -- "
                            else acc ++ x) "" (group s)


addSpacesBetweenTokens :: String -> [String]
addSpacesBetweenTokens s = 
    words . reverse $ foldl(\acc x -> 
    if x == '(' 
    || x == ')' 
    || x == '{' 
    || x == '}' 
    || x == ';' 
    || x == '~' 
    || x == '-' 
    || x == '+' 
    || x == '*' 
    || x == '/' 
    || x == '|'
    || x == '&'
    || x == '^'
    || x == '%' then ' ' : x : ' ' : acc else x:acc ) "" (addSpacesBetweenTokens' s)
