module Types where

data Token =
  Lambda Char | CharTok Char | Dot | Space | LParen | RParen | Eq | Name String
  deriving (Show, Eq)

data Term =
  Var Char | Abs Char Term | Ap Term Term
