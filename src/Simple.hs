module Simple where

-- simple recursion
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- pattern matching
myTake _ [] = []
myTake n _
        | n <= 0 = []
myTake n (x:xs) = [x] ++ myTake (n-1) xs        

-- lambda
gt3 xs = filter (\x -> x > 3) xs

gt3_2 xs = filter (> 3) xs

-- function composition
plus1Double x = ( (*2) . (+1) ) x

-- 'let' and 'in'
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = filter (<=x) xs
        larger = filter (>x) xs   
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- list-comprehension    
pairs n = [ k | i <- [1..n], j <- [1..n], i /= j, let k = [i,j]] 
-- list-comprehension with monad
pairs2 n = do
    i <- [1..n]
    j <- [1..n]
    -- guard (i /= j)
    let k = [i,j]
    return k


-- my filter, otherwise
myFilter :: (a->Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs

-- myMap
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


main :: IO ()
main = do

    putStrLn (show (pairs 5))
    putStrLn (show (pairs2 5))
    putStrLn (show (myMap (+2) [-2,1,3,-6]))
    putStrLn (show (myFilter (>0) [-2,1,3,-6]))
    putStrLn (show (quicksort [6,2,4,8,5,7,1,6,4,7,5,8,9]))
    putStrLn (show (plus1Double 5))
    putStrLn (show (gt3 [1 .. 5]))
    putStrLn (show (gt3_2 [1 .. 5]))
    putStrLn (show (myTake 3 [1 .. 5]))
    putStrLn (show (mySum [1 .. 5]))

    -- automatic currying
    let add x y  = x + y
    let add3 = add 3    
    putStrLn (show (add3 5))    
    
    putStrLn "Have a good day!"