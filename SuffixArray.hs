module SuffixArray (
    SuffixArray(..)
    , suffixArrayBy
    , suffixArray
    , fromList
    , toList
    , vecElems
    , binarySearch
) where

import qualified Data.Vector as V
import qualified Data.List as L

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int) deriving Show

saCompare :: (Ord a) => (a -> a -> Ordering) ->
                            V.Vector a -> Int -> Int -> Ordering
--saCompare cmp d a b = cmp (V.drop a d) (V.drop b d)
saCompare cmp d a b = cmp (d V.! a) (d V.! b)


suffixArrayBy :: (Ord a) => (a -> a -> Ordering) -> V.Vector a -> SuffixArray a
suffixArrayBy cmp dataVec = SuffixArray dataVec (V.fromList srtIndex)
    where upperBound = V.length dataVec - 1
          usrtIndex = [0..upperBound]
          srtIndex = L.sortBy (saCompare cmp dataVec) usrtIndex


suffixArray :: Ord a => V.Vector a  -> SuffixArray a
suffixArray vec = suffixArrayBy compare vec

-- Since most of datatypes have fromList and toList functions, let's define them too
fromList :: Ord a => [a] -> SuffixArray a
fromList = suffixArray . V.fromList

toList :: SuffixArray a -> [[a]]
toList (SuffixArray d i) = V.foldr vecAt [] i
    where vecAt idx l = V.toList (V.drop idx d) : l

-- get elements of suffixarray as vector of vectors
vecElems :: SuffixArray a -> V.Vector (V.Vector a)
vecElems (SuffixArray d i) = V.map vecAt i
    where vecAt ind = V.drop ind d

data Debug a = Correct a | Wrong String deriving(Show)
-- binary search function (bounded)
binarySearchByBounded :: Ord a => (a -> a -> Ordering) -> V.Vector a -> a -> Int -> Int -> Debug Int
binarySearchByBounded cmp v elem lower upper 
    | V.null v = Wrong "Null"
    | upper < lower = Wrong "u<l"
    | otherwise = case elem `compare` midElem of
        EQ -> Correct midIndex
        LT -> binarySearchByBounded cmp v elem lower (midIndex-1)
        GT -> binarySearchByBounded cmp v elem (midIndex+1) upper
    where midIndex = (lower + upper) `div` 2
          midElem = v V.! midIndex

-- wrappers for binary search
binarySearchBounded :: Ord a => V.Vector a -> a -> Int -> Int -> Debug Int
binarySearchBounded v e l h = binarySearchByBounded compare v e l h

binarySearch :: Ord a => V.Vector a -> a -> Debug Int
binarySearch v e = binarySearchBounded v e 0 (V.length v - 1)

main = do
    --putStrLn $ show $ suffixArray $ V.fromList $ words "hello my name is bibek pandey"
    putStrLn $ show $ fromList $ words "to be or not"
    putStrLn $ show $ toList $ fromList $ words "to be or not"
