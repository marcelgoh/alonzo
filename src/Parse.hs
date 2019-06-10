-- Lexing and parsing functionality

module Parse where

import qualified Data.Char as C

import AssocList
import Types

-- given token list, output Term if syntactically valid, Nothing otherwise
parseTerm :: [(String, Term)] -> [Token] -> Maybe Term
parseTerm assocList tokList =
  case shunt [] [] tokList of
    Nothing -> Nothing
    Just shunted ->
      case toTree shunted of
        Nothing -> Nothing
        Just (term, rest) -> Just term
  where
    -- use shunting-yard algorithm to put tokens in Polish notation
    shunt opStack outStack [] = Just (combine opStack outStack)
      where
        combine [] outs = outs
        combine (o : os) outs = combine os (o : outs)
    shunt opStack outStack (t : ts) =
      case t of
        Lambda c  -> shunt opStack (Lambda c : outStack) ts
        CharTok c -> shunt opStack (CharTok c : outStack) ts
        Name s    -> shunt opStack (Name s : outStack) ts   -- treat name as atom
        Dot       -> case opStack of
                       -- application has higher precedence than abstraction
                       (Space : ops) -> shunt ops (Space : outStack) (t : ts)
                       -- push dot onto operator stack
                       _             -> shunt (Dot : opStack) outStack ts
        Space     -> case opStack of
                       -- application is left-associative
                       (Space : ops) -> shunt ops (Space : outStack) (t : ts)
                       _             -> shunt (Space : opStack) outStack ts
        LParen    -> shunt (LParen : opStack) outStack ts
        RParen    -> case opStack of
                       []             -> Nothing   -- unbalanced parentheses
                       (LParen : ops) -> shunt ops outStack ts  -- annihilation
                       (op:ops)       -> shunt ops (op : outStack) (t : ts)
    -- turn shunted tree into a parse tree
    toTree = buildOneExpr
      where
        buildOneExpr [] = Nothing
        buildOneExpr (t : ts) =
          case t of
            Dot ->
              case buildOneExpr ts of
                Just (term, (Lambda c : rest)) -> Just (Abs c term, rest)
                _                              -> Nothing
            Space ->
              case buildOneExpr ts of
                Just (term1, rest) ->
                  case buildOneExpr rest of
                      Just (term2, rest') -> Just (Ap term2 term1, rest')
                      _                   -> Nothing
                _ -> Nothing
            CharTok c -> Just (Var c, ts)
            Name s ->
              case searchKeys assocList s of
                Just term -> Just (term, ts)
                Nothing -> Nothing
            _ -> Nothing

-- parse one line of tokens into a statement
parseStmt :: [(String, Term)] -> [Token] -> Maybe Stmt
parseStmt aList [Name s] =
  Just (NameStmt s)
parseStmt aList (Name s : Eq : tokens) =
  case parseTerm aList tokens of
    Just term -> Just (AssigStmt s term)
    Nothing -> Nothing
parseStmt aList tokens =
  case parseTerm aList tokens of
    Just term -> Just (TermStmt term)
    Nothing -> Nothing

-- given string, output token list if inputs correct, Nothing if invalid
tokenise :: String -> Maybe [Token]
tokenise s = trimRedundant $ iter [] s
  where
    -- remove redundant spaces from final token list
    trimRedundant Nothing = Nothing
    trimRedundant (Just l) = iter [] l
      where
        iter acc [] = Just (reverse acc)
        iter acc (t : ts) =
          case t of
            Space -> case (acc, ts) of
                       ([], _) -> iter acc ts
                       (_, []) -> iter acc ts
                       (a : _, t' : _) ->
                         case (a, t') of
                           -- only leave spaces in when needed
                           (RParen, LParen)       -> iter (Space : acc) ts
                           (CharTok _, CharTok _) -> iter (Space : acc) ts
                           (CharTok _, LParen)    -> iter (Space : acc) ts
                           (RParen, CharTok _)    -> iter (Space : acc) ts
                           (Name _, Name _)       -> iter (Space : acc) ts
                           (Name _, LParen)       -> iter (Space : acc) ts
                           (RParen, Name _)       -> iter (Space : acc) ts
                           (Name _, CharTok _)    -> iter (Space : acc) ts
                           (CharTok _, Name _)    -> iter (Space : acc) ts
                           _                      -> iter acc ts
            _     -> iter (t : acc) ts
    -- remove all leading spaces from a string
    trimSpaces [] = []
    trimSpaces (c : cs) =
      case c of
        ' ' -> trimSpaces cs
        _   -> (c : cs)
    -- extract all leading uppercase letters in string
    getName = iter []
      where
        iter acc "" = (reverse acc, "")
        iter acc (c : cs) =
          if C.isUpper c then
            iter (c : acc) cs
          else (reverse acc, c : cs)
    -- iterate through chars in string and build token list
    iter acc "" = Just (reverse acc)
    iter acc (c : cs) =
      case c of
        '\x3BB' -> case cs of
                     []         -> Nothing
                     (c' : cs') -> if C.isLower c' then
                                     iter ((Lambda c') : acc) cs'
                                   else Nothing
        '\\'    -> case cs of   -- apparently multiple patterns not allowed
                     []         -> Nothing
                     (c' : cs') -> if C.isLower c' then
                                     iter ((Lambda c') : acc) cs'
                                   else Nothing
        '.'     -> iter (Dot : acc) cs
        ' '     -> iter (Space : acc) (trimSpaces cs)
        '('     -> iter (LParen : acc) cs
        ')'     -> iter (RParen : acc) cs
        '='     -> iter (Eq : acc) cs
        _       -> if C.isLower c then
                     iter ((CharTok c) : acc) cs
                   else if C.isUpper c then
                          case getName (c : cs) of
                            (name, rest) -> iter ((Name name) : acc) rest
                        else Nothing

