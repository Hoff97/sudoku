module Check where

import           Sudoku
import           Test.QuickCheck

prop_revapp :: [Int] -> Bool
prop_revapp xs = if length xs == 9 then replicate 9 xs == (fromBlocks $ blocks $ replicate 9 xs) else True

test :: IO ()
test = quickCheck prop_revapp
