import BinaryTree(Tree(EmptyTree), treeInsertAs, treeElem, depthFirstList)
import SuffixArray( SuffixArray(..), suffixArray)
import qualified Data.Vector as V

str = "hello my name is bibek pandey haha"

main = putStrLn $ show $ suffixArray $ words str
