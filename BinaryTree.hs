lmodule BinaryTree(
    Tree(EmptyTree)
    , treeInsert
    , treeInsertAs
    , treeElem
    , depthFirstList
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Read, Show, Eq)

treeInsert :: (Ord a) => a -> (Tree a) -> (Tree a)
treeInsert val EmptyTree = Node val EmptyTree EmptyTree
treeInsert val (Node ref left right)
    | val <= ref = Node ref (treeInsert val left) right
    | otherwise = Node ref left (treeInsert val right)

treeElem :: (Ord a) => a -> (Tree a) -> Bool
treeElem _ EmptyTree = False
treeElem val (Node ref left right)
    | val == ref = True
    | val < ref = treeElem val left
    | val > ref = treeElem val right

treeInsertAs :: Ord a => (a -> a -> Ordering) -> a->  (Tree a) -> (Tree a)
treeInsertAs fn item EmptyTree = Node item EmptyTree EmptyTree
treeInsertAs fn item (Node ref left right) 
    | item `fn` ref == GT = Node ref left (treeInsertAs fn item right)
    | otherwise = Node ref (treeInsertAs fn item left) right

depthFirstList :: (Tree a) -> [a]
depthFirstList EmptyTree = []
depthFirstList (Node val EmptyTree EmptyTree) = [val]
depthFirstList (Node val left right) = depthFirstList left ++ [val] ++  depthFirstList right
num=3
nums = [16,8,5,4,2,9,7,10,24,14,1]
tree :: Tree Int
tree = foldr treeInsert EmptyTree nums

main = do 
    putStrLn $ show $ tree
    putStrLn $ show $ treeElem 99 tree
