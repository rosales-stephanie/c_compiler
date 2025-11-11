module Parser (
    parseProgram
) where 

import System.Exit 
import Tokens
import qualified Ast
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, isNothing)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

-- Using StateT over Writer monad, wrapped with MaybeT to handle Nothing
type Parser a = MaybeT (StateT [Token] (Writer String)) a

expect :: [Token] -> Parser [Token]
expect expected = do
    given <- get
    case given of 
        [] -> do 
            lift . lift $ tell $ "Error: expected " ++ (show $ map show expected)
            MaybeT $ return Nothing
        _ -> let toksToTest = take (length expected) given
                 restOfToks = drop (length expected) given 
             in if expected /= toksToTest then do
                    lift . lift $ tell $ "Error: expected \
                    \" ++ (show $ map show expected) ++ "\
                    \ but got " ++ (show $ map show toksToTest) ++ "\n"
                    MaybeT $ return Nothing
                else do 
                    lift $ put restOfToks
                    return $ expected
                    

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

parseId :: Parser String
parseId = do 
    tokens <- get
    if tokens == [] then do
        lift . lift $ tell "Error: expected Identifier but got [];\n"
        MaybeT $ return Nothing
    else 
        case listToMaybe tokens of 
            Just (Identifier name) -> do 
                lift $ put $ drop 1 tokens
                return $ name
            _ -> do
                lift $ put tokens
                lift . lift $ tell $ "Error: expected Identifier but got\
                                \ " ++ (show $ listToMaybe tokens) ++ ";\n"
                MaybeT $ return Nothing


parseExp :: Maybe Ast.Exp -> Int -> Parser Ast.Exp
parseExp left minPrec = do
    tokens <- get
    if tokens == [] then
        if isNothing left then do
            lift . lift $ tell "Error: something has gone terribly wrong..." 
            MaybeT $ return Nothing
        else do
            lift . lift $ tell "Error: you're probably missing a semicolon\n"
            MaybeT $ return Nothing
    else 
        let Just peek = listToMaybe tokens
        in case left of
            Nothing -> do
                factor <- parseFactor
                if isFactor peek
                    -- maybe (3 + 2) or 1;
                    then parseExp (Just factor) minPrec
                else if peek `elem` binOps 
                    -- maybe -1 + 2; or +2; or ~1;
                    then return factor
                else do
                    -- if there's no left expression, no factor,
                    -- and no binary operator...
                    -- maybe it's a CloseParen or a Semicolon?
                    lift . lift $ tell "Error: what? no left exp..?\n"
                    MaybeT $ return Nothing
            Just e -> do
                if isFactor peek then do 
                    lift . lift $ tell "Error: exp [missing binOp] factor\n"
                    MaybeT $ return Nothing
                else if peek `elem` binOps then 
                    -- with a valid left expression take a look at the right side
                    let binOp    = parseBinOp peek
                        currPrec = Map.findWithDefault 0 binOp precMap
                    in if currPrec >= minPrec then do
                           lift $ put $ drop 1 tokens
                           exp <- parseExp Nothing (currPrec + 1) -- right
                           parseExp (Just (Ast.Binary binOp e exp)) 
                                                      minPrec
                       else 
                           return $ e -- precedence climbing in action!
                else do
                   -- valid left expression
                   -- parseFactor can take the CloseParen or Semicolon
                   return $ e


parseFactor :: Parser Ast.Exp
parseFactor = do
    tokens <- get
    let nextToken = listToMaybe tokens
        toks      = drop 1 tokens
    case nextToken of
        Nothing -> do 
            lift . lift $ tell "Error: empty list in parseFactor\n"
            MaybeT $ return Nothing
        Just tok -> case tok of
            Constant num -> do
                lift $ put toks
                return $ Ast.Constant num
            OpenParen -> do
                lift $ put toks
                exp <- parseExp Nothing 0
                failed <- expect [CloseParen]
                return exp
            _ -> if tok == Tilde || tok == Hyphen then do
                    lift $ put toks
                    exp <- parseFactor
                    return $ Ast.Unary Ast.Negate exp
                 else do
                    lift . lift $ tell $ "Error: No match for " ++ show tok ++ "\n"
                    MaybeT $ return Nothing


parseStatement :: Parser Ast.Statement
parseStatement = do
    expect [KeywordReturn] 
    exp <- parseExp Nothing 0
    skippedExp <- get
    expect [Semicolon]
    return $ Ast.Return exp

    
parseFunc :: Parser Ast.FuncDef
parseFunc = do
    expect [KeywordInt]
    funcName <- parseId
    afterParseId <- get
    expect [OpenParen, KeywordVoid, CloseParen, OpenBracket]
    statement <- parseStatement
    afterParseStatement <- get
    expect [CloseBracket]
    return $ Ast.FuncDef {Ast.name=funcName, Ast.body=statement}


parseProgram :: Parser Ast.Program
parseProgram = do 
    func <- parseFunc
    leftovers <- get
    case leftovers of
        [] -> return $ Ast.Program func
        _  -> do
            lift . lift $ tell $ "Error: parseProgram had leftovers \
                \" ++ (show leftovers) ++ "\n"
            MaybeT $ return Nothing
