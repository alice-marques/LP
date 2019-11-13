splitString :: String -> String
splitString (x:xs)
      | xs == [] = []
      | x == fecha  = splitString xs
      | otherwise = (x:splitString xs)
      where (fecha) = (']')


foo :: [Int] -> Int
foo [] = 0
foo (x:xs) = x + foo xs

checa1a10 :: [Int] -> [Bool]
checa1a10 (xs) = [x `elem` xs | x <- [1..9]]


checa1a10' :: [Bool] -> Bool
checa1a10' [] = True
checa1a10' (x:xs) = x && checa1a10' xs
