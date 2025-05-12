module Ast
( 
  Expression(..),
  Return(..),
  Statement(..),
  FuncDef,
  Program
) where

import Tokens

data Expression = Expression Token deriving (Show)
data Return = Return Expression deriving (Show)
data Statement = Statement Return deriving (Show)
data FuncDef = FuncDef { name :: Token, body :: Statement} deriving (Show)
data Program = Program FuncDef deriving (Show)
