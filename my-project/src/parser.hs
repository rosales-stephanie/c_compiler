module Parser (
    validParse,
    errors
) where 

import Text.Regex.PCRE
import System.Exit 
import Tokens
import qualified Lexer


validParse :: [Token] -> Bool
validParse toks = 
    let errArr = errors toks 
    in if length errArr > 0 then False else True


errors :: [Token] -> [String]
errors xs = 
    let errArr = foldl(\acc f -> (f $ xs) : acc) [] checks
    in foldl(\acc x -> case x of 
                           Left s -> s : acc
                           _ -> acc) [] errArr


checks :: [[Token] -> Either String Bool]
checks = [checkParens, checkBrackets, checkTokens, checkIdentifiers]


keywords :: [Token]
keywords = [KeywordInt, KeywordVoid, KeywordReturn]


checkIdentifiers :: [Token] -> Either String Bool
checkIdentifiers [] = Right True 
checkIdentifiers (x : xs) = case x of 
                                Identifier id -> case () of
                                    _ | id =~ "(?i)return" -> Left $ "Error: " ++ id 
                                      | id =~ "(?i)int" -> Left $ "Error: " ++ id 
                                      | id =~ "(?i)void" -> Left $ "Error: " ++ id 
                                      | otherwise -> checkIdentifiers xs
                                _ -> checkIdentifiers xs


checkTokens :: [Token] -> Either String Bool
checkTokens [x] = if x /= CloseBracket 
                      then Left $ "Error: " ++ (show x) 
                  else Right True
checkTokens (x : s : xs) = 
    case x of 
        Identifier "main" -> Left "Error: missing int main"
        KeywordInt -> case s of  
                          Identifier "main" -> checkTokens xs
                          Identifier _ -> checkTokens (s : xs)
                          _ -> Left $ "Error: int " ++ (show s)
        Identifier _ -> case s of 
                            Identifier _ -> Left "Error: consecutive identifiers"
                            _ -> checkTokens (s : xs)
        _ -> case s of
                 CloseBracket -> if x /= Semicolon 
                                       then Left "Error: missing semicolon"
                                   else checkTokens (s : xs)
                 _ -> if ((elem x keywords) && (elem s keywords)) 
                          then Left "Error: consectutive keywords"
                      else checkTokens (s : xs)


checkParens :: [Token] -> Either String Bool
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
    in if num /= 0 then Left "Error: Invalid parenthesis" else Right True


checkBrackets :: [Token] -> Either String Bool
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
    in if num /= 0 then Left "Error: Invalid brackets" else Right True
