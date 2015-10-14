module Sudoku where

import           Control.Applicative ((<$>))
import           Control.Arrow       (first)
import           Data.List           (findIndex)
import           Debug.Trace
import           Matrix
import           Safe

data Field = Set Int | Possible [Int] deriving (Eq,Show,Read)

type Sudoku = Matrix Field

start = replicate 9 $ replicate 9  $ Possible [1..9]

fromGiven :: [[Maybe Int]] -> Sudoku
fromGiven = map (map f)
    where
        f (Just a) = Set a
        f _ = Possible [1..9]

toString :: Sudoku -> String
toString = concatMap ((++"\n") . map field)
    where
        field (Set x) = head $ show x
        field _ = '_'

fromString :: String -> [[Maybe Int]]
fromString x = map (map (readMay . (:[]))) $ lines x

fromFile :: FilePath -> IO Sudoku
fromFile path = do
    s <- readFile path
    return $ fromGiven $ fromString s

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

indices2D :: (a -> Bool) -> [[a]] -> Maybe (Int,Int)
indices2D _ [] = Nothing
indices2D f (x:xs) = case findIndex f x of
    Just i  -> return (0,i)
    _       -> first (+1) <$> indices2D f xs

allPosAt :: Sudoku -> (Int,Int) -> [Sudoku]
allPosAt s (i,j) = map (\x -> update2 i j x s) pos
    where
        pos = case s!!i!!j of
            Possible x  -> map Set x
            Set x       -> [Set x]

solve :: Sudoku -> [Sudoku]
solve x
    | solved step       = [step]
    | impossible step   = []
    | otherwise         = case indices2D (not . isSet) step of
        Just i  -> concatMap solve $ allPosAt step i
        _       -> []
    where
        step = reduceSudoku x
