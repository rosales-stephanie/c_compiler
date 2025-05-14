module Parser (
    validParse,
    errors,
    parseProgram
) where 

import Text.Regex.PCRE
import System.Exit 
import Tokens
import qualified Lexer
import qualified Ast


{-
parseProgram
parseFunction
parseExp 
parseStatement
expect
ast -> assembly ast
assembly ast -> file aka code emission
-}


expect :: [Token] -> [Token] -> Either String [Token]
expect expected given = 
    let toksToTest = take (length expected) given
        restOfToks = drop (length expected) given 
    in case given of
        [] -> Left $ "Error: expected " ++ (show $ map show expected)
        _ -> if expected /= toksToTest
                 then Left $ "Error: expected \
                 \ " ++ (show $ map show expected) ++ "\
                 \ but got " ++ (show $ map show toksToTest)
             else Right restOfToks


parseId :: [Token] -> ([Token], Either String String)
parseId [] = ([], Left $ "Error: expected Identifier but got [];\n")
parseId (tok : xs) = 
    case tok of 
        Identifier n -> (xs, Right n)
        err -> ((tok : xs), Left $ "Error: expected Identifier String but got\
        \ " ++ (show err) ++ ";\n")


parseInt :: [Token] -> ([Token], Either String Ast.Constant)
parseInt [] = ([], Left $ "Error: expected Constant Int but got [];\n")
parseInt (tok : xs) = 
    case tok of
        Tokens.Constant n -> (xs, Right $ Ast.Constant n)
        err -> ((tok : xs), Left $ "Error: expected Constant Int but got\
        \ " ++ (show err) ++ ";\n")


parseExp :: [Token] -> ([Token], Either String Ast.Constant)
parseExp toks = 
    let (skippedInt, int) = parseInt toks
    in case int of
        Right c -> (skippedInt, Right c)
        Left err -> (toks, Left $ "Error: parseExp (int failed);\n" ++ err)


parseStatement :: [Token] -> ([Token], Either String Ast.Return)
parseStatement toks =
    let ret = expect [KeywordReturn] toks
    in case ret of 
        Right skippedRet -> 
            let (skippedExp, exp) = parseExp skippedRet
                retToks           = expect [Semicolon] skippedExp
            in case exp of
                Right e -> case retToks of
                               Right nextToks 
                                   -> (nextToks, Right $ Ast.Return e)
                               Left err 
                                   -> (skippedExp, Left $ "Error:\
                                   \ parseStatement (retToks failed);\n" ++ err)
                Left err -> (skippedExp, Left $ "Error: parseStatement\
                                        \ (exp failed);\n" ++ err)
        Left err -> (toks, Left $ "Error: parseStatement (ret failed);\n" ++ err) 
    

parseFunc :: [Token] -> ([Token], Either String Ast.FuncDef)
parseFunc toks =
    let ret = expect [KeywordInt] toks
    in case ret of
        Right skippedKeyWdInt -> 
            let (skippedId, id) = parseId skippedKeyWdInt
                skippedToks     = expect [OpenParen, KeywordVoid, CloseParen, OpenBracket] skippedId
            in case id of
                Right n -> 
                    case skippedToks of
                        Right nextToks ->
                            let (skippedStatement, statement) = parseStatement nextToks 
                                retToks                       = expect [CloseBracket] skippedStatement
                            in case statement of
                                Right state -> 
                                    case retToks of
                                        Right nextToks 
                                            -> (nextToks, Right (Ast.FuncDef {Ast.name=n, Ast.body=state}))
                                        Left err 
                                            -> (skippedStatement, Left $ "Error:\
                                                \ parseFunc (retToks failed)\
                                                \;\n" ++ err)
                                Left err -> (skippedStatement, Left $ "Error:\
                                \ parseFunc (statement failed);\n" ++ err)
                        Left err -> (skippedId, Left $ "Error:\
                                            \ parseFunc (skippedToks failed);\n" ++ err)
                Left err -> (toks, Left $ "Error:\
                                    \ parseFunc (id failed);\n" ++ err)
        Left err -> (toks, Left $ "Error: parseFunc (ret failed);\n" ++ err)


parseProgram :: [Token] -> Either String Ast.Program
parseProgram toks = 
    let (skippedMain, func) = parseFunc toks
    in case func of
        Right f -> case skippedMain of 
                       [] -> Right $ Ast.Program f
                       leftovers -> Left $ "Error: parseProgram had leftovers\
                                     \ " ++ (show leftovers)
        Left err -> Left $ "Error: parseProgram (func failed);\n" ++ err


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
