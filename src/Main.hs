module Main where

import           Sudoku
import           System.CPUTime
import           Util

main :: IO ()
main = do
    putStrLn "Pleas give an input file"
    file <- getLine
    sudoku <- fromFile file
    case solve sudoku of
        solution:_  -> do
            putStrLn "At least one solution found, writing to file"
            diagramWindowed $ toDiagram solution
        _           -> putStrLn "No Solution found"
