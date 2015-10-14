module Sudoku where

import           Matrix

data Field = Set Int | Possible [Int] deriving (Eq,Show,Read)

type Sudoku = Matrix Field

start = replicate 9 $ replicate 9  $ Possible [1..9]

fromGiven :: [[Maybe Int]] -> Sudoku
fromGiven = map (map f)
    where
        f (Just a) = Set a
        f _ = Possible [1..9]

isSet :: Field -> Bool
isSet (Set _) = True
isSet _ = False

solved :: Sudoku -> Bool
solved = all (all isSet)

impossible :: Sudoku -> Bool
impossible = any (elem $ Possible [])

reduce :: [Field] -> [Field]
reduce x = map reduced x
    where
        reduced (Possible l) = Possible $ filter (`notElem` set) l
        reduced x = x
        set = foldl setVals [] x
        setVals l (Set x) = x:l
        setVals l _ = l

reduceSudoku :: Sudoku -> Sudoku
reduceSudoku = fromBlocks . map reduce . blocks . columns . map reduce . columns . map reduce

setOnes :: Sudoku -> Sudoku
setOnes = map (map one)
    where
        one (Possible [a]) = Set a
        one x = x

subbed :: Int -> [a] -> [[a]]
subbed _ [] = []
subbed x l = if x > length l then [] else take x l:subbed x (drop x l)

blocks :: Matrix a -> [[a]]
blocks x = concatMap blocked $ subbed 3 x
    where
        blocked x = map concat $ subbed 3 $ columns x

fromBlocks :: [[a]] -> Matrix a
fromBlocks x = concatMap deblocked $ subbed 3 x
    where
        deblocked x = map concat $ columns $ map(columns . subbed 3) x
