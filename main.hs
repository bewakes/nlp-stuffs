import BinaryTree(Tree(EmptyTree), treeInsertAs, treeElem, depthFirstList)
import SuffixArray( SuffixArray(..), suffixArray, suffixArraytoList, binarySearchSA, getNgramCount)
import qualified Data.Vector as V
import Helpers(binarySearch, binaryGetCount)

str = "hello my name is bibek pandey haha"
sa = suffixArray $ words str
lst = suffixArraytoList sa

nums = [1,2,3,3,3,3,4,4,9]

main = do
    putStrLn $ show $ binaryGetCount 4 nums 0 (length nums - 1)
