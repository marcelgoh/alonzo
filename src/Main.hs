-- Front-end for the interpreter

import qualified Control.Monad as Ctrl
import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified System.IO as SysIO

import Types

-- given token list, output Term if syntactically valid, Nothing otherwise
parseTerm :: [Token] -> Maybe [Token]
parseTerm tokList = shunt [] [] tokList
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
    -- toTree tokList =
      

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
                           (CharTok _, CharTok _) -> iter (Space : acc) ts
                           (CharTok _, LParen)    -> iter (Space : acc) ts
                           (RParen, LParen)       -> iter (Space : acc) ts
                           (RParen, CharTok _)    -> iter (Space : acc) ts
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
        '\\' -> case cs of
                  []         -> Nothing
                  (c' : cs') -> if C.isLower c' then
                                  iter ((Lambda c') : acc) cs'
                                else Nothing
        '.'  -> iter (Dot : acc) cs
        ' '  -> iter (Space : acc) (trimSpaces cs)
        '('  -> iter (LParen : acc) cs
        ')'  -> iter (RParen : acc) cs
        '='  -> iter (Eq : acc) cs
        _    -> if C.isLower c then
                  iter ((CharTok c) : acc) cs
                else if C.isUpper c then
                       case getName (c : cs) of
                         (name, rest) -> iter ((Name name) : acc) rest
                     else Nothing

-- REPL
loop :: IO ()
loop = do
  putStr "]=> "
  SysIO.hFlush SysIO.stdout
  input <- getLine
  let tokens = tokenise input
  putStrLn $ show tokens
  Ctrl.when (M.isNothing tokens) $ do putStrLn "Invalid input."
                                      loop
  let shunted = parseTerm $ M.fromJust tokens
  Ctrl.when (M.isNothing shunted) $ do putStrLn "Parse error."
                                       loop
  putStrLn $ show shunted
  loop

-- entry point
main :: IO ()
main = do
  putStrLn "+----------------------------------------------+"
  putStrLn "|        ALONZO \x3BB-CALCULUS INTERPRETER         |"
  putStrLn "|   Author: Marcel Goh (Release: 07.05.2019)   |"
  putStrLn "|            Type \"Ctrl-C\" to quit.            |"
  putStrLn "+----------------------------------------------+"
  loop

