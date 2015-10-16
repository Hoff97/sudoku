module Sudoku where

import           Control.Applicative    ((<$>))
import           Control.Arrow          (first)
import           Data.List              (findIndex)
import           Debug.Trace
import           Diagrams.Backend.Cairo
import           Diagrams.Core.Envelope
import           Diagrams.Prelude
import           Matrix
import           Safe

-- | A Field either has a set number or some possible numbers in it
data Field = Set Int | Possible [Int] deriving (Eq,Show,Read)

-- | A Sudoku is a twodimensional list of Fields
type Sudoku = Matrix Field

-- | The starting sudoku is empty, giving every field the possibility to contain any number from 1 to 9.
start = replicate 9 $ replicate 9  $ Possible [1..9]

-- | Constructs a Sudoku from a Matrix of given numbers.
fromGiven :: Matrix (Maybe Int) -> Sudoku
fromGiven = map (map f)
    where
        f (Just a) = Set a
        f _ = Possible [1..9]

-- | Shows the sudoku line by line, unsets fields are shown as underscores.
toString :: Sudoku -> String
toString = concatMap ((++"\n") . map field)
    where
        field (Set x) = head $ show x
        field _ = '_'

-- | Reads a sudoku from a string.
fromString :: String -> [[Maybe Int]]
fromString x = map (map (readMay . (:[]))) $ lines x

-- | Reads a sudoku from a file
fromFile :: FilePath -> IO Sudoku
fromFile path = do
    s <- readFile path
    return $ fromGiven $ fromString s

-- | Gives, wether a field is set or not.
isSet :: Field -> Bool
isSet (Set _) = True
isSet _ = False

-- | Checks wether all fields of the sudoku are set.
--   It does however not check, wether all conditions for a solved Sudoku are given.
solved :: Sudoku -> Bool
solved = all (all isSet)

-- | Checks, wether a given Sudoku is impossible to solve.
impossible :: Sudoku -> Bool
impossible = any (elem $ Possible [])

-- | Reduces the possibilities of all non-set fields in a list of fields, so they only contain numbers not set by other elements in the list.
reduce :: [Field] -> [Field]
reduce x = map reduced x
    where
        reduced (Possible l) = Possible $ filter (`notElem` set) l
        reduced x = x
        set = foldl setVals [] x
        setVals l (Set x) = x:l
        setVals l _ = l

-- | Reduces the non-set fields of a sudoku, so the numbers they contain match the sudoku conditions.
reduceSudoku :: Sudoku -> Sudoku
reduceSudoku = fromBlocks . map reduce . blocks . columns . map reduce . columns . map reduce

-- | Splits a list into sublists of a given length, wich when concatenatet will be the original list.
--   Example:
--   subbed 3 [1,2,3,4,5,6,7,8,9] = [[1,2,3],[4,5,6],[7,8,9]]
subbed :: Int -> [a] -> [[a]]
subbed _ [] = []
subbed x l = if x > length l then [] else take x l:subbed x (drop x l)

-- | Yields the all the 9x9 blocks a sudoku is divided into.
blocks :: Matrix a -> [[a]]
blocks x = concatMap blocked $ subbed 3 x
    where
        blocked x = map concat $ subbed 3 $ columns x

-- | Constructs a susoku from the 9x9 blocks it is divided into.
fromBlocks :: [[a]] -> Matrix a
fromBlocks = concatMap deblocked . subbed 3
    where
        deblocked = map concat . columns . map(columns . subbed 3)

-- | Yields the indices of the first element in a twodimensional list to match a condition.
--   Example:
--   indices2D (==5) [[1,1,1],[1,1,5],[1,1,1]] = Just (1,2)
indices2D :: (a -> Bool) -> [[a]] -> Maybe (Int,Int)
indices2D _ [] = Nothing
indices2D f (x:xs) = case findIndex f x of
    Just i  -> return (0,i)
    _       -> first (+1) <$> indices2D f xs

-- | All the possible sudokus, by setting the possible values at one position in the sudoku.
allPosAt :: Sudoku -> (Int,Int) -> [Sudoku]
allPosAt s (i,j) = map (\x -> update2 i j x s) pos
    where
        pos = case s!!i!!j of
            Possible x  -> map Set x
            Set x       -> [Set x]

-- | Solves a sudoku, giving all possible solutions.
--   At least one field shouldnt be set, the correctnes of a already solved sudoku wont be checked.
solve :: Sudoku -> [Sudoku]
solve x
    | solved step       = [step]
    | impossible step   = []
    | otherwise         = case indices2D (not . isSet) step of
        Just i  -> concatMap solve $ allPosAt step i
        _       -> []
    where
        step = reduceSudoku x

-- | Draws a sudoku to a main diagram.
toDiagram x = foldl (===) mempty $ map (foldl (|||) mempty . map field) x
    where
        field (Set a) = text (show a) # scale 30 <> square 40
        field _ = square 40
