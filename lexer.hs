import Data.Char
import System.IO
import System.Exit (exitWith, ExitCode(..))
import Tokens

main :: IO() = do
    contents <- getContents
    --prettyPrint contents
    --putStrLn . show . validTokens . createTokens $ contents
    let err = validTokens . createTokens $ contents
    if not err then prettyPrint contents else prettyPrint' contents
    if not err then exitWith (ExitFailure 2) else exitWith (ExitSuccess)


validTokens :: [Either String Tokens.Token] -> Bool
validTokens = foldl(\acc x -> case x of Left _ -> False
                                        Right _ -> acc) True


createTokens :: String -> [Either String Tokens.Token]
createTokens output = reverse $ 
    (foldl(\acc x -> createToken x : acc) [] (seperateTokens $ output))


prettyPrint' :: String -> IO ()
prettyPrint' output = 
    let eitherTokens = foldl(\acc x -> createToken x : acc) [] (seperateTokens $ output)
        tokens = foldl(\acc x -> case x of
                                    Left _ -> acc
                                    Right tok -> (show tok) : acc) [] eitherTokens
    in putStr . unlines $ tokens 


prettyPrint :: String -> IO ()
prettyPrint output = putStr . unlines . reverse $ 
    (foldl(\acc x -> 
        show (createToken x) : acc) [] (seperateTokens $ output))


createToken :: String -> Either String Tokens.Token
createToken s
    | s == "int"                       = Right Tokens.KeyWordInt
    | s == "void"                      = Right Tokens.KeyWordVoid
    | s == "return"                    = Right Tokens.KeyWordReturn
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
