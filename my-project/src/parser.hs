module Parser (
    parseProgram
) where 

import System.Exit 
import Tokens
import qualified Ast


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
        wrongTok -> ((tok : xs), Left $ "Error: expected Identifier but got\
        \ " ++ (show wrongTok) ++ "\n")


parseInt :: [Token] -> ([Token], Either String Ast.Constant)
parseInt [] = ([], Left $ "Error: expected Constant but got []\n")
parseInt (tok : xs) = 
    case tok of
        Tokens.Constant n -> (xs, Right $ Ast.Constant n)
        wrongTok -> ((tok : xs), Left $ "Error: expected Constant but got\
        \ " ++ (show wrongTok) ++ "\n")


parseExp :: [Token] -> ([Token], Either String Ast.Exp)
parseExp toks = 
    let (skippedInt, int) = parseInt toks
    in case int of
        Right c -> (skippedInt, Right $ Ast.Exp c)
        Left err -> (toks, Left $ "Error: parseExp (int failed);\n" ++ err)


parseStatement :: [Token] -> ([Token], Either String Ast.Statement)
parseStatement toks =
    let ret = expect [KeywordReturn] toks
    in case ret of 
        Right skippedRet -> 
            let (skippedExp, exp) = parseExp skippedRet
                retToks           = expect [Semicolon] skippedExp
            in case exp of
                Right e -> case retToks of
                               Right nextToks 
                                   -> (nextToks, Right $ Ast.Statement $ Ast.Return e)
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
