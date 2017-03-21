module Matrix ()
where

import qualified Data.Vector as V
import qualified Data.List as L

data Matrix a = Matrix (V.Vector (V.Vector a)) deriving (Eq)

-- Size of a matrix
size :: Matrix a -> (Int, Int)
size (Matrix m) = (length m, length (V.head m))

-- Create a matrix with given value and size
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix r c v = Matrix $ V.fromList (
        take r ( repeat (V.fromList ( take c ( repeat v)))))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix r c = createMatrix r c 0

identityMatrix :: Int -> Matrix Float
identityMatrix 0 = Matrix (V.fromList [])
identityMatrix n = Matrix (V.fromList $ getRows 0 n)
    where getRows ind size
            | ind >=size = []
            | otherwise = V.fromList (oneAtPos ind size) : getRows (ind Prelude.+ 1) size
          oneAtPos pos size = [booltoint (x == pos) | x <- [0..(size-1)]]
          booltoint v | v==True = 1
                      | otherwise = 0

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
          addZipped (a, b) = a Prelude.+ b
-- multiplication of matrices
mat1@(Matrix m1) * mat2@(Matrix m2) = [ [mult x y | y <- cols mat2] | x <- matToLst mat1]
    where mult a b = sum $ V.map (\(x,y)->x Prelude.* y) $ V.zip a b
          cols mat@(Matrix m) = matToLst $transpose mat
          matToLst mat@(Matrix m) = V.toList m


-- transpose of a matrix
transpose :: Matrix a -> Matrix a
transpose (Matrix m) = Matrix $ V.fromList $ getCols (V.toList m) 0
    where getCols mat ind
            | V.length m < ind = []
            | otherwise = (V.fromList $ map (V.! ind) mat) : getCols mat (ind Prelude.+ 1)


