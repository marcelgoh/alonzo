-- Front-end for the interpreter

import qualified Control.Monad as Ctrl
import qualified System.IO as SysIO
import qualified Data.Maybe as M

import Parse

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
  let stmt = parseStmt $ M.fromJust tokens
  Ctrl.when (M.isNothing stmt) $ do putStrLn "Parse error."
                                    loop
  putStrLn $ show stmt
  loop

-- entry point
main :: IO ()
main = do
  putStrLn "+----------------------------------------------+"
  putStrLn "|        ALONZO \x3BB-CALCULUS INTERPRETER         |"
  putStrLn "|   Author: Marcel Goh (Release: 19.05.2019)   |"
  putStrLn "|            Type \"Ctrl-C\" to quit.            |"
  putStrLn "+----------------------------------------------+"
  loop

