module Types where

data Token = Lambda | Var Char | Dot | Space | LParen | RParen
  deriving (Show, Eq)
