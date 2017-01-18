import BinaryTree(Tree(EmptyTree), treeInsertAs, treeElem, depthFirstList)
import SuffixArray( SuffixArray(..), suffixArray, suffixArraytoList, binarySearchSA, getNgramCount)
import qualified Data.Vector as V
import Helpers(binarySearch, binaryGetCount)

str = "hello my name is bibek pandey haha"
sa = suffixArray $ words str
lst = suffixArraytoList sa

nums = [1,2,2,4,4,4,4,4,8,8,9,9,9,9,10,10]

main = do
    putStrLn $ show $ binaryGetCount 10 nums 0 (length nums - 1)
