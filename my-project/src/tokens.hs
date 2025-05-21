module Tokens
( 
  Token(..)
) where

data Token = OpenParen 
            | CloseParen 
            | OpenBracket 
            | CloseBracket 
            | Semicolon 
            | KeywordInt 
            | KeywordVoid 
            | KeywordReturn 
            | Constant Int
            | Identifier String 
            | Tilde 
            | Hyphen
            | DecrementOp deriving (Read, Eq)


instance Show Token where
    show OpenParen = "("
    show CloseParen = ")" 
    show OpenBracket = "{" 
    show CloseBracket = "}"
    show Semicolon = ";" 
    show KeywordInt = "int"
    show KeywordVoid = "void" 
    show KeywordReturn = "return"
    show (Constant n) = show n
    show (Identifier s) = s
    show Tilde = "~"
    show Hyphen = "-"
    show DecrementOp = "--"
