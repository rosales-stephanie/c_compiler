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
                Left err -> (toks, Left $ err ++ "Error: parseExp consInt;\n")
        OpenParen -> 
            let (nextToks, innerExp) = parseExp toks
                expCloseParen        = expect [CloseParen] nextToks 
            in case innerExp of
                Right exp 
                    -> case expCloseParen of
                           Left err -> (nextToks, Left $ err ++ "Error:\
                                           \ parseExp expCloseParen;\n")
                           Right nextToks -> (nextToks, Right exp)
                Left err 
                    -> (nextToks, Left $ err ++ "Error: parseExp innerExp;\n")
        Tilde ->
            let (nextToks, innerExp) = parseExp toks
            in case innerExp of
                Right exp -> (nextToks, Right $ Ast.Unary Ast.Complement exp)
                Left err  -> (nextToks, Left $ err ++ "Error: parseExp ~;\n")
        Hyphen ->
            let (nextToks, innerExp) = parseExp toks
            in case innerExp of
                Right exp -> (nextToks, Right $ Ast.Unary Ast.Negate exp)
                Left err  -> (nextToks, Left $ err ++ "Error: parseExp -;\n") 
        _          -> ((nextToken:toks), Left "Error: parseExp (no match);\n")


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
                            -> (skippedExp, Left $ err ++ "Error:\
                                  \ parseStatement expSemicolon;\n")
                Left err 
                    -> (skippedExp, Left $ err ++ "Error: parseStatement exp;\n")
        Left err -> (toks, Left $ err ++ "Error: parseStatement nextToks;\n") 
    

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
                                            -> (skippedStatement, Left $ err ++ "Error:\
                                            \ parseFunc expCloseBracket;\n")
                                Left err -> (skippedStatement, Left $ err ++ "Error:\
                                \ parseFunc statement;\n")
                        Left err -> (skippedId, Left $ err ++ "Error:\
                                            \ parseFunc skippedToks;\n")
                Left err -> (toks, Left $ err ++ "Error:\
                                    \ parseFunc id;\n")
        Left err -> (toks, Left $ err ++ "Error: parseFunc expInt;\n")


parseProgram :: [Token] -> Either String Ast.Program
parseProgram toks = 
    let (skippedMain, func) = parseFunc toks
    in case func of
        Right f -> 
            case skippedMain of 
                [] -> Right $ Ast.Program f
                leftovers -> Left $ "Error: parseProgram had leftovers\
                                                 \ " ++ (show leftovers)
        Left err -> Left $ err ++ "Error: parseProgram\
                            \ func;\n" ++ (show skippedMain) ++ "\n"
