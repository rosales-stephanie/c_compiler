import System.Exit (exitWith, ExitCode(..))
import Tokens

main :: IO() = do
    contents <- getContents
    let arr = reverse . foldl(\acc x -> (read x :: Tokens.Token) : acc) [] $ lines contents
        parens = validParens arr
    --putStrLn . show $ checkParens arr 0
    --printTokens arr
    case parens of 
        Left s  -> putStrLn s
        Right _ -> printTokens arr 
    case parens of 
        Left s  -> exitWith (ExitFailure 2)
        Right _ -> exitWith (ExitSuccess)
    

--preProcessTokens :: [Tokens.Token] -> Either String Bool





printTokens :: [Tokens.Token] -> IO()
printTokens = putStr . unlines . reverse . foldl(\acc x -> (show x) : acc) []


validParens :: [Tokens.Token] -> Either String Bool
validParens s
    | not $ checkParens s 0 = Left "Invalid Parenthesis"
    | not $ checkBrackets s 0 = Left "Invalid Brackets"
    | otherwise             = Right True


checkParens :: [Tokens.Token] -> Int -> Bool
checkParens [] num = if num == 0 then True else False
checkParens (x : xs) num = 
    let count = case x of 
                    OpenParen -> num + 1
                    CloseParen -> num - 1
                    _ -> num
    in if count < 0 then False else checkParens xs count


checkBrackets :: [Tokens.Token] -> Int -> Bool
checkBrackets [] num = if num == 0 then True else False
checkBrackets (x : xs) num = 
    let count = case x of 
                    OpenBracket -> num + 1
                    CloseBracket -> num - 1
                    _ -> num
    in if count < 0 then False else checkBrackets xs count
