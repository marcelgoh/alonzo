-- Types used in the program

module Types where

import qualified Data.Map as Map
import qualified Data.List as List

data Token =
  Lambda Char | CharTok Char | Dot | Space | LParen | RParen | Eq | Name String
  deriving (Show, Eq)

data Term =
  Var Char | Abs Char Term | Ap Term Term
--  deriving Show

instance Show Term where
  show (Var x) = [x]
  show (Abs x t) = "(" ++ ('\x3BB' : x : '.' : (show t)) ++")"
  show (Ap t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

instance Eq Term where
  t1 == t2 = alphaEquiv' t1 t2
    where
      alphaEquiv' t u = walk' t [] u []
        where
          -- equivalent bound variables
          walk' (Var v) e (Var w) f = List.elemIndex v e == List.elemIndex w f
          -- shadow previous names in recursive step
          walk' (Abs v t) e (Abs w u) f = walk' t (v : e) u (w : f)
          -- recurse down both terms
          walk' (Ap t1 t2) e (Ap u1 u2) f = walk' t1 e u1 f && walk' t2 e u2 f
          -- all other structures fail
          walk' _ _ _ _ = False

-- gets a list of all free variables in a term
freeVars :: Term -> [Char]
freeVars t =
  case t of
    (Var x) -> [x]
    (Abs x t') -> [ y | y <- freeVars t', y /= x]
    (Ap t1 t2) -> freeVars t1 ++ freeVars t2

-- get a variable not free in either of t1 or t2
getFresh :: Term -> Term -> Char
getFresh t1 t2 =
  let free1 = freeVars t1
      free2 = freeVars t2
      getChar [] = error "Exhausted all variables"
      getChar (x : xs) =
        if x `elem` free1 || x `elem` free2 then getChar xs else x
  in getChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01233456789"

-- true if the variable x is free in term t
isFreeIn :: Char -> Term -> Bool
isFreeIn x t =
  case t of
    (Var y) -> y == x
    (Abs y t') -> if y == x then False else isFreeIn x t'
    (Ap t1 t2) -> isFreeIn x t1 || isFreeIn x t2

-- performs M [x := N]
subst :: Term -> Char -> Term -> Term
subst m x n =
  case m of
    (Var y) -> if y == x then n else Var y
    (Ap p q) -> Ap (subst p x n) (subst q x n)
    (Abs y p) ->
      if y == x then
        Abs y p  -- x not free
      else
        if y `isFreeIn` n then
          let z = getFresh p n   -- get a fresh variable z not in FV(P) or FV(N)
          in Abs z (subst (subst p y (Var z)) x n)
        else
          Abs y (subst p x n)

-- perform a single beta reduction step if possible, or return Nothing
-- if maxDepth reached, return Nothing (catches terms with no normal form)
reduce1 :: Int -> Term -> Maybe Term
reduce1 depth term =
  if depth > 300 then
    Nothing
  else
    case term of
      (Var x) -> Nothing
      (Abs y p) ->
        case reduce1 (depth + 1) p of
          Just p' -> Just (Abs y p')
          Nothing -> Nothing
      (Ap (Abs x m) n) -> Just (subst m x n)
      (Ap t1 t2) ->
        case reduce1 (depth + 1) t1 of
          Just t' -> Just (Ap t' t2)
          Nothing ->
            case reduce1 (depth + 1) t2 of
              Just t' -> Just (Ap t1 t')
              Nothing -> Nothing

-- repeatedly beta reduce a term (normal-order)
betaReduce :: Term -> Term
betaReduce term =
  case reduce1 0 term of
    Just t -> betaReduce t
    Nothing -> term

data Stmt =
  AssigStmt String Term | TermStmt Term | NameStmt String
  deriving (Show, Eq)

