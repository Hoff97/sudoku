module Main where

import           Sudoku
import           System.CPUTime
import           Util

--TODO: Possibility to input the sudoku via a GUI
-- | Asks for an input file, containing a unsolved sudoku, reads and solves the sudoku and displays the solution in a simple GTK window
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
