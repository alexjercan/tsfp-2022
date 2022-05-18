module Main where

import Syntax.Grammar (parseProgram)
import Evaluation.Big (evalList')
import qualified Evaluation.Normal as N
import qualified Data.Map as M
import Syntax.Expression (Expression)

main :: IO ()
main = do
    parse "prog.txt"
    putStrLn "\n"
    eval "prog.txt"

    putStrLn "===============================\n"

    parse "prog1.txt"
    putStrLn "\n"
    eval "prog1.txt"

parse :: String -> IO ()
parse file = readFile file
     >>= maybe (putStrLn "Syntax error!") (mapM_ print) . parseProgram


eval :: String -> IO ()
eval file = readFile file
     >>= maybe (putStrLn "Syntax error!") evalExpressions . parseProgram

evalExpressions :: [Expression] -> IO ()
evalExpressions xs = do
    result <- evalList' N.evalE xs M.empty
    case result of
      Left _ -> return ()
      Right (xs', _) -> mapM_ print xs'
