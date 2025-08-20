module Parser (
    parseProgram
) where 

import System.Exit 
import Tokens
import qualified Ast
import qualified Data.Map as Map


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


binOps :: [Token]
binOps = [Hyphen, Plus, Asterisk, ForwardSlash, Percent]


factors :: [Token]
factors = [OpenParen, Tilde]


parseBinOp :: Token -> Ast.BinaryOp
parseBinOp tok = 
    case tok of
        Hyphen      -> Ast.Subtract
        Plus        -> Ast.Add  
        ForwardSlash -> Ast.Divide
        Percent     -> Ast.Remainder
        Asterisk    -> Ast.Multiply


precMap :: Map.Map Ast.BinaryOp Int
precMap = Map.fromList [(Ast.Add, 45), (Ast.Subtract, 45), (Ast.Multiply, 50), (Ast.Divide, 50), (Ast.Remainder, 50)] 


parseExp :: Ast.Exp -> [Token] -> Int -> ([Token], Either String Ast.Exp)
parseExp left [] _ = ([], Right left)
parseExp left (peek:toks) minPrec = 
    case peek of
        Constant _ ->
                let (rest, factor) = parseFactor (peek:toks) 
                in case factor of
                    Right factorExp -> parseExp factorExp rest minPrec
                    Left err -> ((peek:toks), Left $ err ++ "Error: parseExp (\n") 
        _ -> if peek `elem` factors then
                let (rest, factor) = parseFactor (peek:toks) 
                in case factor of
                    Right factorExp -> parseExp factorExp rest minPrec
                    Left err -> ((peek:toks), Left $ err ++ "Error: parseExp (\n") 
             else
                if peek `elem` binOps then
                    case left of 
                        Ast.Constant 2147483647 -> (peek:toks, Left "Error: \
                                                    \parseExp missing left;\n")
                        _ ->
                            let binOp    = parseBinOp peek
                                currPrec = Map.findWithDefault 0 binOp precMap
                            in if currPrec >= minPrec then 
                                let (rest, right) = parseExp 
                                            (Ast.Constant 2147483647) toks (currPrec + 1)
                                in case right of
                                    Right (Ast.Constant 2147483647) -> 
                                        (toks, Left "Error: parseExp missing right;\n")
                                    Right exp -> 
                                        let retLeft = Ast.Binary binOp left exp
                                        in parseExp retLeft rest minPrec
                                    Left err -> (rest, Left $ err ++ "\
                                                \Error: parseExp \
                                                \" ++ show left ++ "\
                                                \ " ++ show toks ++ "\
                                                \ " ++ show (currPrec + 1) ++ "\n")
                               else
                                   (peek:toks, Right left)
                else
                   --CloseParen, Semicolon
                   (peek:toks, Right left)


parseFactor :: [Token] -> ([Token], Either String Ast.Exp)
parseFactor (nextToken : toks) = 
    case nextToken of
        Constant num -> (toks, Right $ Ast.Constant num)
        OpenParen -> 
            let (nextToks, innerExp) = parseExp (Ast.Constant 2147483647) toks 0
                expCloseParen        = expect [CloseParen] nextToks 
            in case innerExp of
                Right exp 
                    -> case expCloseParen of
                           Left err -> (nextToks, Left $ err ++ "Error:\
                                           \ parseFactor expCloseParen;\n")
                           Right nextToks -> (nextToks, Right exp)
                Left err
                    -> (nextToks, Left $ err ++ "Error: parseFactor innerExp;\n")
        Tilde ->
            let (nextToks, innerExp) = parseFactor toks
            in case innerExp of
                Right exp -> (nextToks, Right $ Ast.Unary Ast.Complement exp)
                Left err -> (nextToks, Left $ err ++ "Error: parseFactor ~;\n")
        Hyphen ->
            let (nextToks, innerExp) = parseFactor toks
            in case innerExp of
                Right exp -> (nextToks, Right $ Ast.Unary Ast.Negate exp)
                Left err -> (nextToks, Left $ err ++ "Error: parseFactor -;\n") 
        _          -> ((nextToken:toks), Left $ "Error: parseFactor\
                                                \ " ++ show nextToken ++ "\n")


parseStatement :: [Token] -> ([Token], Either String Ast.Statement)
parseStatement toks =
    let nextToks = expect [KeywordReturn] toks
    in case nextToks of 
        Right skippedRet -> 
            let (skippedExp, exp) = parseExp (Ast.Constant 2147483647) skippedRet 0
                expSemicolon           = expect [Semicolon] skippedExp
            in case exp of
                Right e -> 
                    case expSemicolon of
                        Right nextToks 
                            -> (nextToks, Right $ Ast.Return e)
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
--parseProgram :: [Token] -> Either String Ast.Program
--parseProgram toks = Left "None"
