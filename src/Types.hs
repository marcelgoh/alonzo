-- Types used in the program

module Types where

import qualified Data.Map as Map

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
--                case (Map.lookup c1 map1, Map.lookup c2 map2) of
--                  (Just i1, Just i2) ->
--                    -- both variables already bound, if binding equal then recurse
--                    -- if i1 == i2 then walk map1 map2 n t1' t2' else Nothing
--                    -- ^ above step probably wrong, we want to shadow instead
                    walk (Map.insert c1 n map1) (Map.insert c2 n map2) (n + 1) t1' t2'
--                  (Nothing, Nothing) ->
--                    -- bind both variables and recurse
--                    walk (Map.insert c1 n map1) (Map.insert c2 n map2) (n + 1) t1' t2'
--                  (_, _) ->
--                    Nothing  -- one bound, one free, so fail
              (Ap t11 t12, Ap t21 t22) ->
                -- recurse down first term, and use updated maps and n-value to
                --   recurse down second term
                case walk map1 map2 n t11 t21 of
                  Just (map1', map2', n') ->
                    walk map1' map2' n' t12 t22
                  Nothing -> Nothing
              _ -> Nothing  -- structures don't even match so fail

data Stmt =
  AssigStmt String Term | TermStmt Term | NameStmt String
  deriving (Show, Eq)
