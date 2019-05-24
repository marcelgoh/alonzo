-- Front-end for the interpreter

import qualified Control.Monad as Ctrl
import qualified Data.Maybe as Maybe
import qualified System.IO as SysIO
import qualified Text.Printf as Printf

import AssocList
import Parse
import Types

-- REPL
loop :: [(String, Term)] -> IO ()
loop assocList = do
  putStr "]=> "
  SysIO.hFlush SysIO.stdout
  input <- getLine
  let tokens = tokenise input
  putStrLn $ show tokens
  Ctrl.when (Maybe.isNothing tokens) $ do putStrLn "Invalid input."
                                          loop assocList
  let stmt = parseStmt $ Maybe.fromJust tokens
  Ctrl.when (Maybe.isNothing stmt) $ do putStrLn "Parse error."
                                        loop assocList
  case Maybe.fromJust stmt of
    AssigStmt str term ->
      loop $ replacePair assocList str term
    TermStmt term -> do
      putStr $ show term
      case searchValues assocList term of
        Just str -> do
          Printf.printf " -- %s\n" str
        Nothing -> do
          Printf.printf "\n"
    NameStmt str ->
      case searchKeys assocList str of
        Just term -> do
          Printf.printf "%s -- %s\n" (show term) str
        Nothing -> do
          Printf.printf "Name `%s` not found.\n" str
  loop assocList

-- entry point
main :: IO ()
main = do
  if (Abs 'a' (Var 'a')) == (Abs 'b' (Var 'b')) then
    putStrLn "Match."
  else
    putStrLn "No match."
  putStrLn "+----------------------------------------------+"
  putStrLn "|        ALONZO \x3BB-CALCULUS INTERPRETER         |"
  putStrLn "|   Author: Marcel Goh (Release: 19.05.2019)   |"
  putStrLn "|            Type \"Ctrl-C\" to quit.            |"
  putStrLn "+----------------------------------------------+"
  loop []
