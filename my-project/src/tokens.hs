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
            | Identifier String deriving (Show, Read, Eq)
