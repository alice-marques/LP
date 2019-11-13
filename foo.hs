import Data.List
import Data.List.Split

foo :: [Int] -> Int
foo [] = 0
foo (x:xs) = x + foo xs

checa1a10 :: [Int] -> [Bool]
checa1a10 (xs) = [x `elem` xs | x <- [1..9]]


checa1a10' :: [Bool] -> Bool
checa1a10' [] = True
checa1a10' (x:xs) = x && checa1a10' xs

contai :: Int -> [Int] -> Bool
contai a b = a `elem` b

ver :: [[Int]] -> [Int]
ver [] = []
ver xs = xs !! 1

hasEqualLine :: Int -> [Int] -> Bool
hasEqualLine _ [] = False
hasEqualLine i xs = i `elem` xs

hasEqualLine' :: Int -> [[Int]] -> Bool
hasEqualLine' _ [] = False
hasEqualLine' i (x:xs) = ((i `elem` x) || (hasEqualLine' i xs))

hasEqualColumn :: Int -> Int -> [[Int]] -> Bool
hasEqualColumn _ _ [] = False
hasEqualColumn columnNumber number (x:xs) = ((number == x !! columnNumber) || (hasEqualColumn columnNumber number xs))

splitString :: String -> String
splitString (x:xs)
      | xs == [] = []
      | x == fecha && fechavir == head xs = splitString (tail xs)
      | x == fecha  = splitString xs
      | otherwise = (x:splitString xs)
      where (fecha,fechavir) = (']', ',')


transformInt :: [String] -> [Int]
transformInt  ([]) = []
transformInt ("":xs) = 0:transformInt xs
transformInt (x:xs) = (read x::Int)  : transformInt xs



splitOn2 :: String -> [[String]]
splitOn2 xs = map (splitOn ",") (filter (not . null) (splitOn "[" $ splitString xs))

solve :: [Int] -> [Int]
solve (x:xs)


findReplaceElement :: [[Int]] -> (Int, Int)
findReplaceElement  [] = []
findReplaceElement (xs:x)  = 
