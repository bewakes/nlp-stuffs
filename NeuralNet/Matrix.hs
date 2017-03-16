module Matrix ()
where

import qualified Data.Vector as V
import qualified Data.List as L

data Matrix a = Matrix (V.Vector (V.Vector a))

-- Size of a matrix
size :: Matrix a -> (Int, Int)
size (Matrix m) = (length m, length (V.head m))

-- Create a matrix with given value and size
createMatrix :: Int -> Int -> a -> Matrix a
createMatrix r c v = Matrix $ V.fromList (
        take r ( repeat (V.fromList ( take c ( repeat v)))))

zeroMatrix :: Int -> Int -> Matrix Float
zeroMatrix r c = createMatrix r c 0

-- TODO: random element matrix

instance (Show a) => Show (Matrix a) where
    show (Matrix mat) = "[\n" ++ concatenated ++"]"

        where concatenated = foldr (++) "" (map formatter (V.toList mat))
              formatter = \x -> (" ["++ spaceSeparated x ++ "  ]\n")
              spaceSeparated x = foldl (++) "" $ map (\y -> "  "++show y) (V.toList x)
