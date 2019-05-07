-- Front-end for the interpreter

import qualified Data.Char as C
import qualified System.IO as SysIO

import Types

-- given string, output token list
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
                           (RParen, LParen)       -> iter (Space : acc) ts
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
        '\\' -> iter (Lambda : acc) cs
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
  case tokenise input of
    Just l -> putStrLn $ show l
    Nothing -> do putStrLn "Invalid input."
                  loop
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

