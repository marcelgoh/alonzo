-- Types used in the program

module Types where

import qualified Data.Map as Map

data Token =
  Lambda Char | CharTok Char | Dot | Space | LParen | RParen | Eq | Name String
  deriving (Show, Eq)

data Term =
  Var Char | Abs Char Term | Ap Term Term

instance Show Term where
  show (Var x) = [x]
  show (Abs x t) = ('\x3BB' : x : '.' : (show t))
  show (Ap t1 t2) = ((show t1) ++ " " ++ (show t2))

instance Eq Term where
  t1 == t2 = alphaEquiv t1 t2
    where
      alphaEquiv :: Term -> Term -> Bool
      alphaEquiv term1 term2 =
        case walk (Map.empty) (Map.empty) 1 term1 term2 of
          Just _ -> True
          Nothing -> False
        where
          -- walk both trees in parallel, maintaining two (Char, Int) maps
          -- return value of Nothing indicates False, Just _ indicates True
          walk :: Map.Map Char Int -> Map.Map Char Int -> Int -> Term -> Term ->
                    Maybe (Map.Map Char Int, Map.Map Char Int, Int)
          walk map1 map2 n term1 term2 =
            case (term1, term2) of
              (Var c1, Var c2) ->
                case (Map.lookup c1 map1, Map.lookup c2 map2) of
                  (Just i1, Just i2) ->
                    -- equivalent bound variables
                    if i1 == i2 then Just (map1, map2, n) else Nothing
                  (Nothing, Nothing) ->
                    -- free variables must be the same
                    if c1 == c2 then Just (map1, map2, n) else Nothing
                  (_, _) ->
                    Nothing  -- one bound, one free, so fail
              (Abs c1 t1', Abs c2 t2') ->
                    -- shadow previous name in recursive step
                    walk (Map.insert c1 n map1) (Map.insert c2 n map2) (n + 1) t1' t2'
              (Ap t11 t12, Ap t21 t22) ->
                -- recurse down first term, and use updated maps and n-value to
                --   recurse down second term
                case walk map1 map2 n t11 t21 of
                  Just (map1', map2', n') ->
                    walk map1' map2' n' t12 t22
                  Nothing -> Nothing
              _ -> Nothing  -- structures don't even match so fail

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
  in getChar "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

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
reduce1 :: Term -> Maybe Term
reduce1 term =
  case term of
    (Var x) -> Nothing
    (Abs y p) -> reduce1 p
    (Ap (Abs x m) n) -> Just (subst m x n)
    (Ap t1 t2) ->
      case reduce1 t1 of
        Just t' -> Just (Ap t' t2)
        Nothing ->
          case reduce1 t2 of
            Just t' -> Just (Ap t1 t')
            Nothing -> Nothing

-- repeatedly beta reduce a term (normal-order)
betaReduce :: Term -> Term
betaReduce term =
  case reduce1 term of
    Just t -> betaReduce t
    Nothing -> term

data Stmt =
  AssigStmt String Term | TermStmt Term | NameStmt String
  deriving (Show, Eq)

