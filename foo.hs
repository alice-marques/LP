import Data.List
import Data.List.Split

foo :: [Int] -> Int
foo [] = 0
foo (x:xs) = x + foo xs

checa1a10 :: [Int] -> Bool
checa1a10 (xs) = checa1a10' [x `elem` xs | x <- [1..9]]


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

hasNoEqualLine :: Int -> [Int] -> Bool
hasNoEqualLine i xs = not (i `elem` xs)

hasEqualLine' :: Int -> [[Int]] -> Bool
hasEqualLine' _ [] = False
hasEqualLine' i (x:xs) = ((i `elem` x) || (hasEqualLine' i xs))

hasEqualColumn :: Int -> Int -> [[Int]] -> Bool
hasEqualColumn _ _ [] = False
hasEqualColumn columnNumber number (x:xs) = ((number == x !! columnNumber) || (hasEqualColumn columnNumber number xs))


hasNoEqualColumn :: Int -> Int -> [[Int]] -> Bool
hasNoEqualColumn _ _ [] = False
hasNoEqualColumn columnNumber number (x:xs) = not ((number == x !! columnNumber) || (hasEqualColumn columnNumber number xs))


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


solve :: [[Int]] -> [[Int]]
solve null xs = []
solve (x:xs)
      |z == 0 = solve ([y | y <- [1..9], y `notElem` x, hasEqualColumn y ((\(Just i)->i) $ findIndex (==0)) (x:xs)])
      where z =

(\(Just i)->i) a

populateGrid :: [[Int]] -> Int
populateGrid xs = traverse (traverse readCell) . xs
  where
    readCell 0 = Just $ Possible [1..9]
    readCell c
      | c > 0 = Just . Fixed . c
      | otherwise = Nothing

findReplaceElement :: [[Int]] -> (Int, Int)
findReplaceElement  [] = []
findReplaceElement (xs:x)  =


, hasEqualColumn y ((\(Just i)->i) $ findIndex (==0)) x



tt :: [Int] -> [Int]
tt [] = []
tt x:xs
    | x == 0 = [y | y <- [1..9], y `notElem` xs, tt y:xs] 
