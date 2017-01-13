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

nums = [16,8,5,4,2,9,7,10,24,14,13]
tree :: Tree Int
tree = foldr treeInsert EmptyTree nums

main = do 
    putStrLn $ show $ tree
    putStrLn $ show $ treeElem 99 tree
