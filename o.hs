import Prelude
import Data.Maybe


--trocaNum :: [Int] -> [Int]
--trocaNum xs = if 0 `elem` xs then trocaNum $ map  else xs

tt :: [Int] -> [Int]
tt xs
    |0 `elem` xs = trocaNum' $ map (\n -> if n == 0 then head [x | x <- [1..9], x `notElem` xs] else n) xs
    |otherwise = xs

trocaNum' :: [Int] -> [Int]
trocaNum' [] = []
trocaNum' (x:xs) = if x `elem` xs then x:map (\n -> if n == x then 0 else n) xs else x:trocaNum' xs

trocaNum :: [[Int]] -> [[Int]]
trocaNum [] = []
trocaNum xs = map tt xs

--trocaUm :: [Int] -> Int -> [Int]
--trocaUm [] = []
--trocaUm xs x = map (\n -> if n == 0 then )


naotemColuna ::[Int] -> [[Int]] -> Bool
naotemColuna _ [[]] = True
naotemColuna l [] = True
naotemColuna l (x:xs) = null (compare l x) && naotemColuna l xs where compare l1 l2 = [i | (i, x, y) <- zip3 [0..] l1 l2, x == y]

mapOnce :: (a -> Maybe a) -> [a] -> [a]
mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
        Nothing -> x : mapOnce f xs
        Just y  -> y : xs

--oo :: [[Int]] -> [[Int]]
--oo xs = do{
--
--}

dirty :: [Int]-> Int -> [Int]
dirty [] _ = []
dirty (x:xs) n = map (\m -> if m == 0 && n `notElem` (x:xs) then n else m) (x:xs)
