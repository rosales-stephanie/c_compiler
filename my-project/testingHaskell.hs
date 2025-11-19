import Data.List (group)


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
