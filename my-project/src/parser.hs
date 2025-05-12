module Parser (
    validParse,
    errors
) where 

import Text.Regex.PCRE
import System.Exit 
import Tokens
import qualified Lexer
import Ast


--ast -> assembly ast
--assembly ast -> file aka code emission


{-
parseProgram
parseFunction
parseExp 
parseStatement
expect
-}


--return Ast.FuncDef
parseFunction :: [Token] -> ([Token], Expression)
parseFunction arr = ([OpenParen], Expression $ Constant 0)



--returning Ast.Program
parseProgram :: [Token] -> Return
parseProgram toks = 
    let (nextToks, exp) = parseFunction toks
    in Return exp


validParse :: [Token] -> Bool
validParse toks = 
    let errArr = errors toks 
    in if length errArr > 0 then False else True


errors :: [Token] -> [String]
errors xs = 
    let errArr = foldl(\acc f -> (f $ xs) : acc) [] checks
    in foldl(\acc x -> case x of 
                           "Success" -> acc
                           s -> s : acc) [] errArr


checks :: [[Token] -> String]
checks = [checkParens, checkBrackets, checkTokens, checkIdentifiers]


keywords :: [Token]
keywords = [KeywordInt, KeywordVoid, KeywordReturn]


checkIdentifiers :: [Token] -> String
checkIdentifiers [] = "Success" 
checkIdentifiers (x : xs) = case x of 
                                Identifier id -> case () of
                                    _ | id =~ "(?i)return" -> "Error: " ++ id 
                                      | id =~ "(?i)int" -> "Error: " ++ id 
                                      | id =~ "(?i)void" -> "Error: " ++ id 
                                      | otherwise -> checkIdentifiers xs
                                _ -> checkIdentifiers xs


checkTokens :: [Token] -> String
checkTokens (x : s : xs) = 
    case x of 
        Identifier "main" -> "Error: missing int main"
        KeywordInt -> case s of  
                          Identifier "main" -> checkTokens xs --skip main
                          Identifier _ -> checkTokens (s : xs)
                          _ -> "Error: int " ++ (show s)
        Identifier _ -> case s of 
                            Identifier _ -> "Error: consecutive identifiers"
                            _ -> checkTokens (s : xs)
        _ -> case s of
                 CloseBracket -> if x /= Semicolon 
                                     then "Error: missing semicolon"
                                 else 
                                     --last token should be }
                                     if xs /= [] then "Error: " ++ (show x)
                                     else "Success"
                 _ -> if ((elem x keywords) && (elem s keywords)) 
                          then "Error: consectutive keywords"
                      else checkTokens (s : xs)


checkParens :: [Token] -> String
checkParens xs = 
    let num = foldl(\acc x ->
                case acc of
                    -1 -> -1
                    _ ->
                        let count = case x of 
                                        OpenParen  -> acc + 1
                                        CloseParen -> acc - 1
                                        _                 -> acc
                        in if (count < 0) then -1 else count) 0 xs
    in if num /= 0 then "Error: Invalid parenthesis" else "Success"


checkBrackets :: [Token] -> String
checkBrackets xs = 
    let num = foldl(\acc x ->
                case acc of
                    -1 -> -1
                    _ ->
                        let count = case x of 
                                        OpenBracket  -> acc + 1
                                        CloseBracket -> acc - 1
                                        _                 -> acc
                        in if (count < 0) then -1 else count) 0 xs
    in if num /= 0 then "Error: Invalid brackets" else "Success"
