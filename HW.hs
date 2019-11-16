import Data.List
import Data.List.Split
import Data.Monoid



data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row  = [Cell] -- trecho extraido de http://bit.ly/2NMvA4o

type Grid = [Row]

t :: Int -> Cell
t x = Fixed x

ff ::[Int]-> [Int] -> [Int]
ff [] us = []
ff (x:xs) us
    | x == 0 = ff (f xs ([y | y <- [1..9], y `notElem` us]) us) us
    | otherwise = x:ff xs us

f :: [Int] -> [Int] -> [Int] -> [Int]
f [] [] us= []
f xs (y:ys) us= if y `notElem` us then do y:xs
                                          y:us else xs



h :: [Int] -> [Int] -> [Int]
h (x:xs) (y:ys) = zipWith (\n k->if x == k then x else k) (x:xs) (y:ys)


tt :: [Int] -> [Int]
tt [] = []
tt (x:xs)
    | 0 `elem` (x:xs) = [y | y <- [1..9], y `notElem` xs]
    | otherwise = [99,99,99,99]

tRow :: [Int] -> Row
tRow [] = []
tRow (x:xs) = (Fixed x):tRow xs

hasEqualLine :: Int -> [Int] -> Bool
hasEqualLine _ [] = False
hasEqualLine i xs = i `elem` xs

hasNoEqualLine :: Int -> [Int] -> Bool
hasNoEqualLine i xs = i `notElem` xs

hasEqualLine' :: Int -> [[Int]] -> Bool
hasEqualLine' _ [] = False
hasEqualLine' i (x:xs) = i `elem` x || hasEqualLine' i xs

hasNoEqualColumn :: Int -> Int -> [[Int]] -> Bool
hasNoEqualColumn _ _ [] = True
hasNoEqualColumn number columnNumber (x:xs) = not (number == x !! columnNumber || hasNoEqualColumn number columnNumber xs)


hasEqualColumn :: Int -> Int -> [[Int]] -> Bool
hasEqualColumn _ _ [] = False
hasEqualColumn columnNumber number (x:xs) = ((number == x !! columnNumber) || (hasEqualColumn columnNumber number xs))




splitString :: String -> String
splitString (x:xs)
      | null xs = []
      | x == fecha && fechavir == head xs = splitString (tail xs)
      | x == fecha  = splitString xs
      | otherwise = x:splitString xs
      where (fecha,fechavir) = (']', ',')


foo :: [Int] -> Int
foo [] = 0
foo (x:xs) = x + foo xs

checa1a9 :: [Int] -> Bool
checa1a9 xs = and [x `elem` xs | x <- [1..9]]


transformInt :: [String] -> [Int]
transformInt  [] = []
transformInt ("":xs) = 0:transformInt xs
transformInt (x:xs) = (read x::Int)  : transformInt xs



splitOn2 :: String -> [[String]]
splitOn2 xs = map (splitOn ",") (filter (not . null) (splitOn "[" $ splitString xs))


solve' :: [Int] -> Int
solve' [] = 0
solve' (x:xs) = x
