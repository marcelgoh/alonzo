-- Types used in the program

module Types where

import qualified Data.Map as M

data Token =
  Lambda Char | CharTok Char | Dot | Space | LParen | RParen | Eq | Name String
  deriving (Show, Eq)

data Term =
  Var Char | Abs Char Term | Ap Term Term
  deriving Show

instance Eq Term where
  t1 == t2 = alphaEquiv t1 t2
    where
      alphaEquiv :: Term -> Term -> Bool
      alphaEquiv term1 term2 =
        case walk (M.empty) (M.empty) 1 term1 term2 of
          Just _ -> True
          Nothing -> False
        where
          -- walk both trees in parallel, maintaining two (Char, Int) maps
          -- return value of Nothing indicates False, Just _ indicates True
          walk :: M.Map Char Int -> M.Map Char Int -> Int -> Term -> Term ->
                    Maybe (M.Map Char Int, M.Map Char Int, Int)
          walk map1 map2 n term1 term2 =
            case (term1, term2) of
              (Var c1, Var c2) ->
                case (M.lookup c1 map1, M.lookup c2 map2) of
                  (Just i1, Just i2) ->
                    -- equivalent bound variables
                    if i1 == i2 then Just (map1, map2, n) else Nothing
                  (Nothing, Nothing) ->
                    -- free variables must be the same
                    if c1 == c2 then Just (map1, map2, n) else Nothing
                  (_, _) ->
                    Nothing  -- one bound, one free, so fail
              (Abs c1 t1', Abs c2 t2') ->
                case (M.lookup c1 map1, M.lookup c2 map2) of
                  (Just i1, Just i2) ->
                    -- both variables already bound, if binding equal then recurse
                    if i1 == i2 then walk map1 map2 n t1' t2' else Nothing
                  (Nothing, Nothing) ->
                    -- bind both variables and recurse
                    walk (M.insert c1 n map1) (M.insert c2 n map2) (n + 1) t1' t2'
                  (_, _) ->
                    Nothing  -- one bound, one free, so fail
              (Ap t11 t12, Ap t21 t22) ->
                -- recurse down first term, and use updated maps and n-value to
                --   recurse down second term
                case walk map1 map2 n t11 t21 of
                  Just (map1', map2', n') ->
                    walk map1' map2' n' t12 t22
                  Nothing -> Nothing
              _ -> Nothing  -- structures don't even match so fail

-- (pretty much arbitrary) ordering for Term type
instance Ord Term where
  compare (Ap _ _) (Ap _ _) = EQ
  compare (Var c1) (Var c2) = compare c1 c2
  compare (Abs c1 _) (Abs c2 _) = compare c1 c2
  compare (Ap _ _) _ = LT
  compare (Abs _ _) _ = GT

data Stmt =
  AssigStmt String Term | TermStmt Term | NameStmt String
  deriving (Show, Eq)
