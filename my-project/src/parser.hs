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

-- Using StateT over Writer monad, wrapped with MaybeT to handle Nothing
type Parser a = MaybeT (StateT [Token] (Writer String)) a

expect :: [Token] -> Parser [Token]
expect expected = do
    given <- get
    case given of 
        [] -> do 
            tell $ "Error: expected " ++ (show $ map show expected)
            MaybeT $ return Nothing
        _ -> let toksToTest = take (length expected) given
                 restOfToks = drop (length expected) given 
             in if expected /= toksToTest then do
                    tell $ "Error: expected \
                    \" ++ (show $ map show expected) ++ "\
                    \ but got " ++ (show $ map show toksToTest) ++ "\n"
                    MaybeT $ return Nothing
                else do 
                    put restOfToks
                    return $ expected
                    

isFactor :: Token -> Bool
isFactor tok =
    case tok of 
        Constant _ -> True
        OpenParen  -> True
        Tilde      -> True
        _          -> False


binOps :: [Token]
binOps = [Hyphen, 
          Plus, 
          Asterisk, 
          ForwardSlash, 
          Percent,
          Carrot,
          Ampersand,
          Pipe,
          GreaterThanGreaterThan,
          LessThanLessThan]


parseBinOp :: Token -> Ast.BinaryOp
parseBinOp tok = 
    case tok of
        Hyphen                 -> Ast.Subtract
        Plus                   -> Ast.Add 
        ForwardSlash           -> Ast.Divide
        Percent                -> Ast.Remainder
        Asterisk               -> Ast.Multiply
        LessThanLessThan       -> Ast.LeftShift
        GreaterThanGreaterThan -> Ast.RightShift
        Pipe                   -> Ast.OR
        Ampersand              -> Ast.AND
        Carrot                 -> Ast.XOR


precMap :: Map.Map Ast.BinaryOp Int
precMap = Map.fromList [(Ast.AND, 20),
                        (Ast.XOR, 15),
                        (Ast.OR, 10),
                        (Ast.RightShift, 40),
                        (Ast.LeftShift, 40),
                        (Ast.Add, 45), 
                        (Ast.Subtract, 45), 
                        (Ast.Multiply, 50), 
                        (Ast.Divide, 50), 
                        (Ast.Remainder, 50)] 

parseId :: Parser String
parseId = do 
    tokens <- get
    if tokens == [] then do
        tell "Error: expected Identifier but got [];\n"
        MaybeT $ return Nothing
    else 
        case listToMaybe tokens of 
            Just (Identifier name) -> do 
                put $ drop 1 tokens
                return $ name
            _ -> do
                put tokens
                tell $ "Error: expected Identifier but got\
                                \ " ++ (show $ listToMaybe tokens) ++ ";\n"
                MaybeT $ return Nothing


parseExp :: Maybe Ast.Exp -> Int -> Parser Ast.Exp
parseExp left minPrec = do
    tokens <- get
    if tokens == [] then
        if isNothing left then do
            tell "Error: something has gone terribly wrong..." 
            MaybeT $ return Nothing
        else do
            tell "Error: you're probably missing a semicolon\n"
            MaybeT $ return Nothing
    else 
        case left of
            Nothing -> do
                factor <- parseFactor
                status <- get
                let Just peek = listToMaybe tokens
                tell $ "Nothing -> " ++ show factor ++ "\n"
                -- maybe (3 + 2) or 1;
                -- maybe -1 + 2; or +2; or ~1;
                parseExp (Just factor) minPrec
            Just e -> do
                let Just peek = listToMaybe tokens
                if isFactor peek then do 
                    tell "Error: exp [missing binOp] factor\n"
                    MaybeT $ return Nothing
                else if peek `elem` binOps then 
                    -- with a valid left expression take a look at the right side
                    let binOp    = parseBinOp peek
                        currPrec = Map.findWithDefault 0 binOp precMap
                    in if currPrec >= minPrec then do
                           put $ drop 1 tokens
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
            tell "Error: empty list in parseFactor\n"
            MaybeT $ return Nothing
        Just tok -> case tok of
            Constant num -> do
                put toks
                tell $ "Constant num " ++ show toks ++ "\n"
                return $ Ast.Constant num
            OpenParen -> do
                put toks
                exp <- parseExp Nothing 0
                failed <- expect [CloseParen]
                return exp
            _ -> if tok == Hyphen then do
                    put toks
                    exp <- parseFactor
                    return $ Ast.Unary Ast.Negate exp
                 else 
                    if tok == Tilde then do
                        put toks
                        exp <- parseFactor
                        return $ Ast.Unary Ast.Complement exp
                    else 
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
            tell $ "Error: parseProgram had leftovers \
                \" ++ (show leftovers) ++ "\n"
            MaybeT $ return Nothing
