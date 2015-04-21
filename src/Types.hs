module Types where

import Control.Exception (assert)

-- polimorphism
data Shape = Circle Float Float Float | Rectange  Float Float Float Float

area :: Shape -> Float
area (Circle _ _ r) = r * r * pi
area (Rectange x1 y1 x2 y2) = (abs $ x1-x2) * (abs $ y1 - y2)

-- record syntax
data Car = Car { model :: String
                , year :: Int
                } deriving (Show)


tellCar (Car { model = "Ford", year = 1973}) = "A '73 Ford"
tellCar (Car { model = m, year = y}) = "A car " ++ m 

-- type parameters
data Maybe a = Just a | Nothing

data Vector a = Vector a a deriving (Show)

-- vplus :: (Num a) => Vector a -> Vector a -> Vector a
Vector x1 y1 `vplus` Vector x2 y2 = Vector (x1 + x2) (y1 + y2)

-- type synomim
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook n pn pb = (n,pn) `elem` pb
    

main :: IO ()
main = do
    -- assert (1 == 2) undefined
    putStrLn ("Rec: " ++ show (area (Rectange 2 3 4 5)))
    putStrLn ("Circle: " ++ show (area (Circle 2 3 4)))
    putStrLn ("Car: " ++ show (Car "Ford" 1968))
    putStrLn (tellCar (Car "Ford" 1968))
    putStrLn (tellCar (Car "Ford" 1973))
    putStrLn (show $ (Vector 3 4) `vplus` (Vector 4 5))

    
