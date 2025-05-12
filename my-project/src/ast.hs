module Ast
( 
  Expression(..),
  Return(..),
  Statement(..),
  FunctionDefinition,
  Program
) where

import Tokens

data Constant = Constant Int deriving (Show)
data Expression = Expression Token deriving (Show)
data Return = Return Expression deriving (Show)
data Statement = Statement Return deriving (Show)
data FunctionDefinition = FunctionDefinition { name :: Token, body :: Statement} deriving (Show)
data Program = Program FunctionDefinition deriving (Show)
