module Matrix where

-- | A Matrix is a list of lines of values.
--   A line is a list of values.
--   Example:
--   a :: Matrix Int
--   a = [[1,2,3],[4,5,6],[7,8,9]]
type Matrix a = [[a]]

-- | Gives the columns of a matrix.
--   Example:
--   columns [[1,2,3],[4,5,6],[7,8,9]] = [[1,4,7],[2,5,8],[3,6,9]]
columns :: Matrix a -> Matrix a
columns = map reverse . foldl (zipWith (flip (:))) (repeat [])

-- | Multiplies two matrixes.
--   Example:
--   [[1,2],[3,4]] `mult` [[5,6],[7,8]] = [[1*5+2*6,1*7+2*8],[3*5+4*6,3*7+4*8]] = [[19,22],[43,50]]
mult :: Num a => Matrix a -> Matrix a -> Matrix a
mult a b = map (\line -> map (sum . zipWith(*) line) $ columns b) a

-- | Zips two lists together using a Function.
--   When the second lists ends, the first is used as the rest of the result list.
--   When the first lists ends, the rest of the result list is empty.
--   Examples:
--   zipA (*) [1,2] [3,4,5] = [1*3,2*4] = [3,8]
--   zipA (*) [1,2,3] [4,5] = [1*4,2*5,3] = [4,10,3]
zipA :: (a -> b -> a) -> [a] -> [b] -> [a]
zipA _ a [] = a
zipA _ [] _ = []
zipA f (x:xs) (y:ys) = f x y:zipA f xs ys

-- | Computes all diagonals of a Matrix.
--   The result is not a Matrix because the lengths varify.
--   Example:
--   diagonals [[1,2,3],[4,5,6],[7,8,9]] = [[1],[2,4],[3,5,7],[6,8],[9],[7],[4,8],[1,5,9],[2,6],[3]]
diagonals :: Matrix a -> [[a]]
diagonals m = diagonalsR m ++ map reverse (diagonalsR (reverse m))

-- | Computes all diagonals going from bottom to top in the rigth dircection.
--   This is used is the diagonals function.
--   Example:
--   diagonals [[1,2,3],[4,5,6],[7,8,9]] = diagonals [[1,2,3],[4,5,6],[7,8,9]] =
diagonalsR :: Matrix a -> [[a]]
diagonalsR [] = []
diagonalsR [a] = map (:[]) a
diagonalsR ([]:_) = []
diagonalsR ((a:as):xs) = [a] : zipA (flip (:)) (diagonalsR xs) as

-- | Makes a matrix with one column from a list of values.
--   Example:
--   vect [1,2,3] = [[1],[2],[3]]
vect :: [a] -> Matrix a
vect = map (:[])

-- | Makes the first column of the matrix to a list of values.
--   Example:
--   toVect [[1],[2],[3]] = [1,2,3]
toVect :: Matrix a -> [a]
toVect [] = []
toVect ([]:_) = []
toVect ((x:_):ys) = x:toVect ys

-- | Creates a Matrix given a dimension and a function taking the line and column and returning the value of the field.
createMatrix :: (Int -> Int -> a) -> Int -> Int -> Matrix a
createMatrix f i j = map (\line -> map (f line) [0..j - 1]) [0..i - 1]

subs :: Int -> [a] -> [[a]]
subs x l@(_:ls) = if x > length l then [] else take x l:subs x ls
subs _ [] = []

update2 :: Int -> Int -> a -> [[a]] -> [[a]]
update2 i j a l = update i updated l
    where
        updated = update j a (l!!i)

update :: Int -> a -> [a] -> [a]
update x a l = take x l ++ [a] ++ drop (x+1) l
