import BinaryTree(Tree(EmptyTree), treeInsertAs, treeElem, depthFirstList)
import SuffixArray( SuffixArray(..), suffixArray, suffixArraytoList, binarySearchSA, getNgramCount, compareSAElements)
import qualified Data.Vector as V
import Helpers(binarySearch, binarySearchAs, binaryGetCount, binaryGetCountAs, strLower)
import Data.Char

str = "hello my name is bibek pandey haha, but my name is not michael. Its okay but not fine, my name "
sa = suffixArray $ map strLower $ words str
lst = suffixArraytoList sa

nums = V.fromList [0,1,1,2,2,2,4,4,4,4,4,4,8,8,9,9,9,10,10, 11, 11]

lstToString :: Show a => [a] -> String
lstToString [] = ""
lstToString (x:xs) = (show x) ++ "\n" ++ (lstToString xs)

main = do
    putStrLn $ lstToString $ V.toList lst
    putStrLn $ show $ binaryGetCountAs compareSAElements ["my", "name", "is"] lst 0 ((length lst) -1)
