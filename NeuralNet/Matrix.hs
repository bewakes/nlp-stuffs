module Matrix (
    Matrix(..)
    , (Matrix.*)
    , (Matrix.+)
    , (Matrix.-)
    , hadamard
    , transpose
    , zeroMatrix
    , zeroMatrixBySize
    , scale
    , size
    , matrixFromList
)
where

import qualified Data.Vector as V
import qualified Data.List as L
import Prelude as P

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq)

-- Size of a matrix
size :: Matrix a -> (Int, Int)
size (Matrix m) = (length m, length (V.head m))

-- Create a matrix with given value and size
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix r c v = Matrix $ V.fromList (
        take r ( repeat (V.fromList ( take c ( repeat v)))))

-- create matrix, input is size, a tuple (numrows, numcols)
createMatrixBySize :: (Int,Int) -> a -> Matrix a
createMatrixBySize (r, c) v = createMatrix r c v

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix r c = createMatrix r c 0

zeroMatrixBySize (r,c) = zeroMatrix r c

-- HADAMARD product (element wise product)
hadamard :: (Num a) => Matrix a -> Matrix a -> Matrix a
hadamard (Matrix m1) (Matrix m2) = Matrix $ V.map row_mult (V.zip m1 m2)
    where row_mult (r1, r2) = V.map (\(x,y) -> x P.* y) $ V.zip r1 r2

identityMatrix :: Int -> Matrix Float
identityMatrix 0 = Matrix (V.fromList [])
identityMatrix n = Matrix (V.fromList $ getRows 0 n)
    where getRows ind size
            | ind >=size = []
            | otherwise = V.fromList (oneAtPos ind size) : getRows (ind P.+ 1) size
          oneAtPos pos size = [booltoint (x == pos) | x <- [0..(size P.-1)]]
          booltoint v = if v==True then 1 else 0

-- Matrix from list
matrixFromList :: [[a]] -> Matrix a
matrixFromList lst = Matrix (V.fromList $ map V.fromList lst)

-- scale matrix
scale :: (Num a) => Matrix a -> a -> Matrix a
scale (Matrix m) f = Matrix (V.map (\x-> V.map (P.* f) x) m)

-- TODO: random element matrix

instance (Show a) => Show (Matrix a) where
    show (Matrix mat) = "[\n" ++ concatenated ++"]"

        where concatenated = foldr (++) "" (map formatter (V.toList mat))
              formatter = \x -> (" ["++ spaceSeparated x ++ "  ]\n")
              spaceSeparated x = foldl (++) "" $ map (\y -> "  "++show y) (V.toList x)

-- addition of matrices
instance (Num a) => Num (Matrix a) where
(Matrix m1) + (Matrix m2) = Matrix $ V.map sumrows (V.zip m1 m2)
    where sumrows (a, b) = V.map addZipped $ V.zip a b
          addZipped (a, b) = a P.+ b
-- multiplication of matrices
(*) :: (Num a) => Matrix a -> Matrix a -> Matrix a
mat1 * mat2 = Matrix $ V.fromList [ V.fromList [mult x y | y <- cols mat2] | x <- matToLst mat1]
    where mult a b = sum $ V.map (\(x,y)->x P.* y) $ V.zip a b
          cols mat = matToLst $transpose mat
          matToLst (Matrix m) = V.toList m

(Matrix m1) - (Matrix m2) = Matrix $ V.map diffrows (V.zip m1 m2)
    where diffrows (a, b) = V.map subtractZipped $ V.zip a b
          subtractZipped (a, b) = a P.- b

-- transpose of a matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix $ V.fromList $ getCols (V.toList m) 0
    where getCols mat ind
            | V.length row > ind = (V.fromList $ map (V.! ind) mat) : getCols mat (ind P.+ 1)
            | otherwise = []
          row = V.head m


-- matrix with random elements
--randomMatrix :: Int -> Int -> IO (Matrix Float)

