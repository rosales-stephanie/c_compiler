module Ast
( 
  Constant(..),
  Return(..),
  FuncDef(..),
  Program(..)
) where

import Tokens

data Constant = Constant Int deriving (Show)
data Return   = Return Constant deriving (Show)
data FuncDef  = FuncDef { name :: String, body :: Return} deriving (Show)
data Program  = Program FuncDef deriving (Show)
