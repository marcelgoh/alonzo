-- Front-end for the interpreter

import qualified System.IO as SysIO

import Types

tokenise :: String -> [Token]
tokenise str = [Lambda, Var 'x', Dot, Space, LParen, RParen]


-- REPL
loop :: IO ()
loop = do
    putStr "]=> "
    SysIO.hFlush SysIO.stdout
    input <- getLine
    putStrLn $ show $ tokenise input
    loop

-- entry point
main :: IO ()
main = do
    putStrLn "+----------------------------------------------+"
    putStrLn "|      ALONZO LAMDBA-CALCULUS INTERPRETER      |"
    putStrLn "|   Author: Marcel Goh (Release: 07.05.2019)   |"
    putStrLn "|            Type \"Ctrl-C\" to quit.            |"
    putStrLn "+----------------------------------------------+"
    loop

