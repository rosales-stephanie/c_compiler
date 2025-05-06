module Main where 

import Text.Regex.PCRE
import System.Exit 
import Tokens

main = do
    contents <- getContents
    let arr = reverse . foldl(\acc x -> (read x :: Tokens.Token) : acc) [] $ lines contents
    putStrLn "Check parens: "
    putStrLn . show $ checkParens arr
    putStrLn "Check brackets: "
    putStrLn . show $ checkBrackets arr
    putStrLn "check identifiers: "
    putStrLn . show $ checkIdentifiers arr
    putStrLn "check tokens: "
    putStrLn . show $ checkTokens arr
    --printTokens arr


keywords :: [Tokens.Token]
keywords = [Tokens.KeywordInt, Tokens.KeywordVoid, Tokens.KeywordReturn]

checkIdentifiers :: [Tokens.Token] -> Either String Bool
checkIdentifiers [] = Right True
checkIdentifiers (x : xs) = case x of 
                                Tokens.Identifier id -> 
                                    if id =~ "(?i)\\breturn\\b" 
                                        then Left $ "Error: " ++ id 
                                    else if id =~ "(?i)\\bint\\b" 
                                        then Left $ "Error: " ++ id 
                                    else if id =~ "(?i)\\bvoid\\b" 
                                        then Left $ "Error: " ++ id 
                                    else checkIdentifiers xs
                                _ -> checkIdentifiers xs


checkTokens :: [Tokens.Token] -> Either String Bool
checkTokens [] = Right True
checkTokens (x : s : xs) = 
    if ((elem x keywords) && (elem s keywords))
        then Left "Error: consectutive identifiers"
    else if s == Tokens.CloseBracket 
        then if x /= Tokens.Semicolon then Left "Error: missing semicolon" 
            else Right True
    else if x == Tokens.KeywordInt
        then case s of  
                Tokens.Identifier _ -> Right True
                _ -> Left $ "Error: int " ++ (show s)
    else if x == Tokens.Identifier "main" then Left "Error: int missing"
    else checkTokens xs


printTokens :: [Tokens.Token] -> IO()
printTokens = putStr . unlines . reverse . foldl(\acc x -> (show x) : acc) []


checkParens :: [Tokens.Token] -> Either String Bool
checkParens xs = 
    let num = foldl(\acc x ->
                case acc of
                    -1 -> -1
                    _ ->
                        let count = case x of 
                                        Tokens.OpenParen  -> acc + 1
                                        Tokens.CloseParen -> acc - 1
                                        _                 -> acc
                        in if (count < 0) then -1 else count) 0 xs
    in if num /= 0 then Left "Error: Invalid parenthesis" else Right True


checkBrackets :: [Tokens.Token] -> Either String Bool
checkBrackets xs = 
    let num = foldl(\acc x ->
                case acc of
                    -1 -> -1
                    _ ->
                        let count = case x of 
                                        Tokens.OpenBracket  -> acc + 1
                                        Tokens.CloseBracket -> acc - 1
                                        _                 -> acc
                        in if (count < 0) then -1 else count) 0 xs
    in if num /= 0 then Left "Error: Invalid parenthesis" else Right True
