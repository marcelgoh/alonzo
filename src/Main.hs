-- Front-end for the interpreter

import qualified Control.Monad as Ctrl
import qualified Data.Maybe as Maybe
import qualified System.IO as SysIO
import qualified Text.Printf as Printf

import AssocList
import Parse
import Types

startingEnv :: [(String, Term)]
startingEnv = [
  ("I", (Abs 'x' (Var 'x'))),
  ("S", (Abs 'x' (Abs 'y' (Abs 'z' (Ap (Ap (Var 'x') (Var 'z')) (Ap (Var 'y') (Var 'z'))))))),
  ("Y", (Abs 'g' (Ap (Abs 'x' (Ap (Var 'g') (Ap (Var 'x') (Var 'x')))) (Abs 'x' (Ap (Var 'g') (Ap (Var 'x') (Var 'x'))))))),
  ("ISZERO", (Abs 'n' (Ap (Ap (Var 'n') (Abs 'x' (Abs 'p' (Abs 'q' (Var 'q'))))) (Abs 'p' (Abs 'q' (Var 'p')))))),
  ("TRUE", (Abs 'p' (Abs 'q' (Var 'p')))),  -- doubles as K-combinator
  ("FALSE", (Abs 'p' (Abs 'q' (Var 'q')))), -- doubles as ZERO
  ("AND", (Abs 'p' (Abs 'q' (Ap (Ap (Var 'p') (Var 'q')) (Var 'p'))))),
  ("OR", (Abs 'p' (Abs 'q' (Ap (Ap (Var 'p') (Var 'p')) (Var 'q'))))),
  ("NOT", (Abs 'p' (Ap (Ap (Var 'p') (Abs 'p' (Abs 'q' (Var 'q')))) (Abs 'p' (Abs 'q' (Var 'p')))))),
  ("IF", (Abs 'p' (Abs 'a' (Abs 'b' (Ap (Ap (Var 'p') (Var 'a')) (Var 'b')))))),
  ("ONE", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Var 'x'))))),
  ("TWO", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Var 'x')))))),
  ("THREE", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x'))))))),
  ("FOUR", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x')))))))),
  ("FIVE", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x'))))))))),
  ("SIX", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x')))))))))),
  ("SEVEN", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x'))))))))))),
  ("EIGHT", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x')))))))))))),
  ("NINE", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x'))))))))))))),
  ("TEN", (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Ap (Var 'f') (Var 'x')))))))))))))),
  ("SUCC", (Abs 'n' (Abs 'f' (Abs 'x' (Ap (Var 'f') (Ap (Ap (Var 'n') (Var 'f')) (Var 'x'))))))),
  ("PRED", (Abs 'n' (Abs 'f' (Abs 'x' (Ap (Ap (Ap (Var 'n') (Abs 'g' (Abs 'h' (Ap (Var 'h') (Ap (Var 'g') (Var 'f')))))) (Abs 'u' (Var 'x'))) (Abs 'u' (Var 'u'))))))),
  ("PLUS", (Abs 'm' (Abs 'n' (Abs 'f' (Abs 'x' (Ap (Ap (Var 'm') (Var 'f')) (Ap (Ap (Var 'n') (Var 'f')) (Var 'x')))))))),
  ("TIMES", (Abs 'm' (Abs 'n' (Abs 'f' (Ap (Var 'm') (Ap (Var 'n') (Var 'f'))))))),
  ("POW", (Abs 'b' (Abs 'e' (Ap (Var 'e') (Var 'b'))))),
  ("CONS", (Abs 'x' (Abs 'y' (Abs 'f' (Ap (Ap (Var 'f') (Var 'x')) (Var 'y')))))),
  ("CAR", (Abs 'p' (Ap (Var 'p') (Abs 'p' (Abs 'q' (Var 'p')))))),
  ("CDR", (Abs 'p' (Ap (Var 'p') (Abs 'p' (Abs 'q' (Var 'q')))))),
  -- SUMREC = \r.\n.(IF (ISZERO n) FALSE (PLUS n (r (PRED n)))) -- (Y SUMREC n) calculates partial sum up to n
  ("SUMREC", (Abs 'r' (Abs 'n' (Ap (Ap (Ap (Ap (Var 'n') (Abs 'x' (Abs 'p' (Abs 'q' (Var 'q'))))) (Abs 'p' (Abs 'q' (Var 'p')))) (Abs 'p' (Abs 'q' (Var 'q')))) (Abs 'f' (Abs 'x' (Ap (Ap (Var 'n') (Var 'f')) (Ap (Ap (Ap (Var 'r') (Abs 'f' (Abs 'x' (Ap (Ap (Ap (Var 'n') (Abs 'g' (Abs 'h' (Ap (Var 'h') (Ap (Var 'g') (Var 'f')))))) (Abs 'u' (Var 'x'))) (Abs 'u' (Var 'u')))))) (Var 'f')) (Var 'x'))))))))),
  -- FACTREC = \r.\n.(IF (ISZERO n) ONE (TIMES n (r (PRED n)))) -- (Y FACTREC n) calculates factorial of n
  ("FACTREC", (Abs 'r' (Abs 'n' (Ap (Ap (Ap (Ap (Var 'n') (Abs 'x' (Abs 'p' (Abs 'q' (Var 'q'))))) (Abs 'p' (Abs 'q' (Var 'p')))) (Abs 'f' (Abs 'x' (Ap (Var 'f') (Var 'x'))))) (Abs 'f' (Ap (Var 'n') (Ap (Ap (Var 'r') (Abs 'f' (Abs 'x' (Ap (Ap (Ap (Var 'n') (Abs 'g' (Abs 'h' (Ap (Var 'h') (Ap (Var 'g') (Var 'f')))))) (Abs 'u' (Var 'x'))) (Abs 'u' (Var 'u')))))) (Var 'f'))))))))
  ]

-- REPL
loop :: [(String, Term)] -> IO ()
loop assocList = do
  putStr "]=> "
  SysIO.hFlush SysIO.stdout
  input <- getLine
  let tokens = tokenise input
  -- putStrLn $ show tokens
  Ctrl.when (Maybe.isNothing tokens) $ do putStrLn "Invalid input."
                                          loop assocList
  let stmt = parseStmt assocList $ Maybe.fromJust tokens
  Ctrl.when (Maybe.isNothing stmt) $ do putStrLn "Parse or name error."
                                        loop assocList
  case Maybe.fromJust stmt of
    AssigStmt str term -> do
      let reduced = betaReduce term
      Printf.printf "%s -- %s\n" (show reduced) str
      loop $ replacePair assocList str reduced
    TermStmt term -> do
      let reduced = betaReduce term
      putStr $ show reduced
      case searchValues assocList reduced of
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
  putStrLn "+----------------------------------------------+"
  putStrLn "|        ALONZO \x3BB-CALCULUS INTERPRETER         |"
  putStrLn "|   Author: Marcel Goh (Release: 11.06.2019)   |"
  putStrLn "|            Type \"Ctrl-C\" to quit.            |"
  putStrLn "+----------------------------------------------+"
  loop startingEnv
