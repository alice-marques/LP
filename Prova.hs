import Data.List

somaN :: [Int] -> Int -> [[Int]]
somaN xs n = removeSameElements $ removeEq $ removeEmpty $ map (take' n (length xs)) (permutations xs) 

somaNn' :: [Int] -> Int
somaNn' [] = 0
somaNn'(x:xs) = x + somaNn' xs

removeEmpty :: [[Int]] -> [[Int]]
removeEmpty [[]] = [[]]
removeEmpty (x:[]) = [x]
removeEmpty (x:xs) 
   | null x = removeEmpty xs
   | otherwise = x : removeEmpty xs

removeEq :: [[Int]] -> [[Int]]
removeEq [[]] = [[]]
removeEq (x:[]) = [x]
removeEq (x:xs)
   | x `elem` xs = removeEq xs
   | otherwise = x : removeEq xs

removeSameElements :: [[Int]] -> [[Int]]
removeSameElements [[]] = [[]]
removeSameElements (x:[]) = [x]
removeSameElements (x:xs) 
   | checkSameE x (head xs) = removeSameElements xs
   | otherwise = x : removeSameElements xs

checkSameE :: [Int] -> [Int] -> Bool
checkSameE xs ys = null (xs \\ ys) && null (ys \\ xs)

take' :: Int -> Int -> [Int] -> [Int]
take' _ 0 _ = []
take' n nTake xs 
   | (somaNn' (take nTake xs)) == n = (take nTake xs)
   | otherwise = take' n (nTake - 1) xs
   
   
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (init xs)


size' :: [a] -> Int
size' [] = 0
size' (x:xs) = 1 + size' xs

dupe :: [a] -> [a]
dupe [] = []
dupe (x:xs) = x:x:dupe xs

penul :: [a] -> a
penul [x] = x
penul xs = last $ init xs

palind :: Eq a => [a] -> Bool
palind [] = True
palind [x] = True
palind (x:xs) = x == (last xs) && palind (tail $ init xs)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = [y | y <- xs, f y]


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys