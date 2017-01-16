module SuffixArray (
    SuffixArray(..)
    , suffixArray
    , suffixArraytoList
    , binarySearchSA
    , getNgramCount
) where

import BinaryTree( Tree(EmptyTree), treeInsertAs, depthFirstList)
import Helpers(binarySearchAs, binarySearch, binaryGetCountAs)
import qualified Data.Vector as V
import qualified Data.List as L

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
    deriving(Show)

compareElemsByIndex :: Ord a => (V.Vector a) -> (Int -> Int -> Ordering)
compareElemsByIndex vec = (\x y-> (V.drop x vec) `compare` (V.drop y vec))

suffixArray :: (Ord a) => [a] -> SuffixArray a
suffixArray [] = SuffixArray (V.fromList []) (V.fromList [])
suffixArray lst = SuffixArray vec (V.fromList dfsList)
    where dfsList = depthFirstList tree
          tree = foldr (treeInsertAs compfn) EmptyTree [0..(length lst -1)]
          compfn = compareElemsByIndex vec
          vec = V.fromList lst

---------------------------
-- TODO -------------------
-- implement binary search (bounded/unbounded)

suffixArraytoList :: SuffixArray a -> [[a]]
suffixArraytoList (SuffixArray d v) = V.foldr listAt [] v
    where listAt i l = V.toList (V.drop i d) : l

-- comparison function for binary search in SA
cmpfn a b = a `compare` (take (length a) b)

-- Binary search
binarySearchSA :: Ord a => [a] -> SuffixArray a -> Int
binarySearchSA elem sa = binarySearchAs cmpfn elem lst 0 (length lst)
    where lst = suffixArraytoList sa

getNgramCount :: Ord a => [a] -> SuffixArray a -> Int
getNgramCount elem sa = binaryGetCountAs cmpfn elem lst 0 len
    where lst = suffixArraytoList sa
          len = length lst
