module Parser (
    parseProgram
) where 

import System.Exit 
import Tokens
import qualified Ast
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, isNothing)
import Control.Monad.State

expect :: [Token] -> State [Token] (Either String ())
expect expected = do
    given <- get
    case given of 
        [] -> return $ Left $ "Error: expected " ++ (show $ map show expected)
        _ -> let toksToTest = take (length expected) given
                 restOfToks = drop (length expected) given 
             in if expected /= toksToTest then
                    return $ Left $ "Error: expected \
                    \" ++ (show $ map show expected) ++ "\
                    \ but got " ++ (show $ map show toksToTest) ++ "\n"
                else do 
                    put restOfToks
                    return $ Right ()
                 

parseId :: State [Token] (Either String String)
parseId = do 
    tokens <- get
    if tokens == [] then 
        return $ Left "Error: expected Identifier but got [];\n"
    else 
        case listToMaybe tokens of 
            Just (Identifier n) -> do 
                put $ drop 1 tokens
                return $ Right n
            _                   -> do
                put tokens
                return $ Left $ "Error: expected Identifier but got\
                                \ " ++ (show $ listToMaybe tokens) ++ ";\n"


binOps :: [Token]
binOps = [Hyphen, Plus, Asterisk, ForwardSlash, Percent]


isFactor :: Token -> Bool
isFactor tok =
    case tok of 
        Constant _ -> True
        OpenParen  -> True
        Tilde      -> True
        _          -> False


parseBinOp :: Token -> Ast.BinaryOp
parseBinOp tok = 
    case tok of
        Hyphen       -> Ast.Subtract
        Plus         -> Ast.Add 
        ForwardSlash -> Ast.Divide
        Percent      -> Ast.Remainder
        Asterisk     -> Ast.Multiply


precMap :: Map.Map Ast.BinaryOp Int
precMap = Map.fromList [(Ast.Add, 45), 
                        (Ast.Subtract, 45), 
                        (Ast.Multiply, 50), 
                        (Ast.Divide, 50), 
                        (Ast.Remainder, 50)] 


parseExp :: Maybe Ast.Exp -> Int -> State [Token] (Either String Ast.Exp)
parseExp left minPrec = do
    tokens <- get
    if tokens == [] then
        if isNothing left then
            return $ Left "what do you mean return nothing...?\n" 
        else 
            return $ Left "you're probably missing a semicolon\n"
    else 
        let Just peek = listToMaybe tokens
        in case left of
            Nothing -> do
                factor <- parseFactor
                case factor of
                    Right factorExp ->
                        if isFactor peek
                            -- maybe (3 + 2) or 1;
                            then parseExp (Just factorExp) minPrec
                        else if peek `elem` binOps -- maybe -1 + 2; or +2; or ~1;
                            then return $ Right factorExp
                        else
                            -- if there's no left expression, no factor,
                            -- and no binary operator...
                            -- maybe it's a CloseParen or a Semicolon?
                            return $ Left "no left exp??\n"
                    Left err -> return $ Left "factorExp failed in parseExp\n"
            Just e ->
                if isFactor peek 
                    then return $ Left "Error: exp [missing binOp] factor\n"
                else if peek `elem` binOps then 
                    -- with a valid left expression take a look at the right side
                    let binOp    = parseBinOp peek
                        currPrec = Map.findWithDefault 0 binOp precMap
                    in if currPrec >= minPrec then do
                           put $ drop 1 tokens
                           rightExp <- parseExp Nothing (currPrec + 1)
                           case rightExp of
                               Right exp -> parseExp (Just (Ast.Binary binOp e exp)) 
                                                      minPrec
                               Left err -> return $ Left $ err ++ "Error: right \
                                          \side of expression failed in parseExp\n"
                       else 
                           return $ Right e -- precedence climbing in action!
                else
                   -- valid left expression
                   -- parseFactor can take the CloseParen or Semicolon
                   return $ Right e


parseFactor :: State [Token] (Either String Ast.Exp)
parseFactor = do
    tokens <- get
    let nextToken = listToMaybe tokens
        toks      = drop 1 tokens
    case nextToken of
        Nothing -> return $ Left "empty list in parseFactor\n"
        Just tok -> case tok of
            Constant num -> do
                put toks
                return $ Right $ Ast.Constant num
            OpenParen -> do
                put toks
                exp <- parseExp Nothing 0
                expect [CloseParen]
                return exp
            _ -> if tok == Tilde || tok == Hyphen then do
                    put toks
                    innerExp <- parseFactor
                    case innerExp of
                        Right exp -> return $ Right $ Ast.Unary Ast.Negate exp
                        Left err  -> return $ Left $ err ++ "Error: \
                                              \innerExp failed in parseFactor;\n"
                 else 
                    return $ Left $ "Error: No match for " ++ show tok ++ "\n"


parseStatement :: State [Token] (Either String Ast.Statement)
parseStatement = do
    expect [KeywordReturn] 
    exp <- parseExp Nothing 0
    skippedExp <- get
    expect [Semicolon]
    case exp of
        Right e -> return $ Right $ Ast.Return e
        Left err -> do
            put skippedExp 
            return $ Left $ err ++ "Error: parseStatement exp;\n"

    
parseFunc :: State [Token] (Either String Ast.FuncDef)
parseFunc = do
    expect [KeywordInt]
    funcName <- parseId 
    afterParseId <- get
    expect [OpenParen, KeywordVoid, CloseParen, OpenBracket]
    case funcName of 
        Right name -> do
            statement <- parseStatement
            afterParseStatement <- get
            expect [CloseBracket]
            case statement of
                Right state -> return $ Right $ Ast.FuncDef {Ast.name=name, Ast.body=state}
                Left err -> do
                    put afterParseStatement 
                    return $ Left $ err ++ "Error: parseFunc statement;\n"
        Left err -> do
            put afterParseId
            return $ Left $ err ++ "Error: parseFunc id;\n"


parseProgram :: [Token] -> Either String Ast.Program
parseProgram toks =
    let (func, skippedMain) = runState parseFunc toks -- (reverse)
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
