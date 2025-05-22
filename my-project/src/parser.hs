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
                 \ but got " ++ (show $ map show toksToTest) ++ "\n"
             else Right restOfToks


parseId :: [Token] -> ([Token], Either String String)
parseId [] = ([], Left $ "Error: expected Identifier but got [];\n")
parseId (tok : xs) = 
    case tok of 
        Identifier n -> (xs, Right n)
        wrongTok -> ((tok : xs), Left $ "Error: expected Identifier but got\
        \ " ++ (show wrongTok) ++ ";\n")


parseInt :: [Token] -> ([Token], Either String Ast.Constant)
parseInt [] = ([], Left $ "Error: expected Constant but got [];\n")
parseInt (tok : toks) = 
    case tok of
        Tokens.Constant n -> (toks, Right $ Ast.Constant n)
        wrongTok -> ((tok : toks), Left $ "Error: expected Constant but got\
        \ " ++ (show wrongTok) ++ ";\n")


parseExp :: [Token] -> ([Token], Either String Ast.Exp)
parseExp (nextToken : toks) = 
    case nextToken of
        Constant _ -> 
            let (skippedInt, consInt) = parseInt (nextToken:toks)
            in case consInt of
                Right c -> (skippedInt, Right $ Ast.Exp c)
                Left err -> (toks, Left $ "Error: parseExp consInt;\n" ++ err)
        OpenParen -> 
            let (nextToks, innerExp) = parseExp toks
                expCloseParen        = expect [CloseParen] nextToks 
            in case innerExp of
                Left err 
                    -> (nextToks, Left $ "Error: parseExp innerExp;\n" ++ err)
                Right exp 
                    -> case expCloseParen of
                           Left err -> (nextToks, Left $ "Error:\
                                           \ parseExp expCloseParen;\n" ++ err)
                           Right nextToks -> (nextToks, Right exp)
        _ -> ((nextToken:toks), Left "Error: parseExp;\n")
      -- nextToken == Tilde || nextToken == Hyphen ->


parseStatement :: [Token] -> ([Token], Either String Ast.Statement)
parseStatement toks =
    let nextToks = expect [KeywordReturn] toks
    in case nextToks of 
        Right skippedRet -> 
            let (skippedExp, exp) = parseExp skippedRet
                expSemicolon           = expect [Semicolon] skippedExp
            in case exp of
                Right e -> 
                    case expSemicolon of
                        Right nextToks 
                            -> (nextToks, Right $ Ast.Statement $ Ast.Return e)
                        Left err 
                            -> (skippedExp, Left $ "Error:\
                                  \ parseStatement expSemicolon;\n" ++ err)
                Left err 
                    -> (skippedExp, Left $ "Error: parseStatement exp;\n" ++ err)
        Left err -> (toks, Left $ "Error: parseStatement nextToks;\n" ++ err) 
    

parseFunc :: [Token] -> ([Token], Either String Ast.FuncDef)
parseFunc toks =
    let expInt = expect [KeywordInt] toks
    in case expInt of
        Right skippedKeyWdInt -> 
            let (skippedId, id) = parseId skippedKeyWdInt
                skippedToks     = expect [OpenParen, KeywordVoid, CloseParen, OpenBracket] skippedId
            in case id of
                Right name -> 
                    case skippedToks of
                        Right nextToks ->
                            let (skippedStatement, statement) = parseStatement nextToks 
                                expCloseBracket               = expect [CloseBracket] skippedStatement
                            in case statement of
                                Right state -> 
                                    case expCloseBracket of
                                        Right nextToks 
                                            -> (nextToks, Right (Ast.FuncDef {Ast.name=name, Ast.body=state}))
                                        Left err 
                                            -> (skippedStatement, Left $ "Error:\
                                            \ parseFunc expCloseBracket;\n" ++ err)
                                Left err -> (skippedStatement, Left $ "Error:\
                                \ parseFunc statement;\n" ++ err)
                        Left err -> (skippedId, Left $ "Error:\
                                            \ parseFunc skippedToks;\n" ++ err)
                Left err -> (toks, Left $ "Error:\
                                    \ parseFunc id;\n" ++ err)
        Left err -> (toks, Left $ "Error: parseFunc expInt;\n" ++ err)


parseProgram :: [Token] -> Either String Ast.Program
parseProgram toks = 
    let (skippedMain, func) = parseFunc toks
    in case func of
        Right f -> 
            case skippedMain of 
                [] -> Right $ Ast.Program f
                leftovers -> Left $ "Error: parseProgram had leftovers\
                                                 \ " ++ (show leftovers)
        Left err -> Left $ "Error: parseProgram\
                            \ func;\n" ++ err ++ (show skippedMain)
