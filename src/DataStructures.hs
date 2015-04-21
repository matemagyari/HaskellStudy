module DataStructures where
import qualified Data.Map as Map

-- map

-- recursive data structures

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a left right)
    | x == a = Node a left right
    | x < a = Node a (treeInsert x left) right
    | otherwise = Node a left (treeInsert x right)

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem EmptyTree _ = False
treeElem (Node y l r) x
    | y == x = True
    | x < y = treeElem l x
    | otherwise = treeElem r x

buildTree :: (Ord a) => [a] -> Tree a
buildTree xs = foldr treeInsert EmptyTree xs 

main = do   
    let t = buildTree [2,5,3,6,8,5,3,7,4]
    putStrLn $ show $ t
    putStrLn $ show $ treeElem t 7
    putStrLn $ show $ treeElem t 1
    
