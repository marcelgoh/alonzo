module Types where

data Token =
  Lambda | CharTok Char | Dot | Space | LParen | RParen | Eq | Name String
  deriving (Show, Eq)

data Term =
  Var Char | Abs Char Term | Ap Term Term
