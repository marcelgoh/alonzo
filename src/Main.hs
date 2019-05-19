-- Front-end for the interpreter

import qualified Control.Monad as Ctrl
import qualified Data.AssocList as AL
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.IO as SysIO
import qualified Text.Printf as Printf

import Parse
import Types

-- REPL
loop :: Map.Map String Term -> [(Term, String)] -> IO ()
loop nameToTerm termToName = do
  putStr "]=> "
  SysIO.hFlush SysIO.stdout
  input <- getLine
  let tokens = tokenise input
  putStrLn $ show tokens
  Ctrl.when (Maybe.isNothing tokens) $ do putStrLn "Invalid input."
                                          loop nameToTerm termToName
  let stmt = parseStmt $ Maybe.fromJust tokens
  Ctrl.when (Maybe.isNothing stmt) $ do putStrLn "Parse error."
                                        loop nameToTerm termToName
  case Maybe.fromJust stmt of
    AssigStmt str term ->
      loop (Map.insert str term nameToTerm) (AL.addEntry term str termToName)
    TermStmt term -> do
      putStr $ show term
      case AL.lookupDef "" term termToName of
        "" -> do
          Printf.printf "\n"
        str -> do
          Printf.printf " -- %s\n" str
    NameStmt str ->
      case Map.lookup str nameToTerm of
        Just term -> do
          Printf.printf "%s -- %s\n" (show term) str
        Nothing -> do
          Printf.printf "Name `%s` not found.\n" str
  loop nameToTerm termToName

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
  loop Map.empty []
