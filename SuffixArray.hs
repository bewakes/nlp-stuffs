module SuffixArray (
    SuffixArray(..)
    , suffixArray
) where

import BinaryTree( Tree(EmptyTree), treeInsertAs, depthFirstList)
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

