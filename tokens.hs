module Tokens 
( 
  Token(..),
--  Expression(..),
--  Return,
--  Statement,
--  FunctionDefinition,
--  Program
) where

type Constant = Int
type Identifier = String

data Token = OpenParen 
            | CloseParen 
            | OpenBracket 
            | CloseBracket 
            | Semicolon 
            | KeyWordInt 
            | KeyWordVoid 
            | KeyWordReturn 
            | Constant Int
            | Identifier String deriving (Show, Read)

--data Expression = Constant
--data Return = Return { value :: Expression }
--data Statement = Statement Return
--data FunctionDefinition = FunctionDefinition { name :: Identifier, body :: Statement}
--data Program = Program { functionDef :: FunctionDefinition }

main = putStr "" 
