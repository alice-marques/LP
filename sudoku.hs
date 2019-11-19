--Andre Carvalho Marques - 15/0005491
--Rodrigo da Silva Navarro - 15/0147376




import Data.List.Split
import Data.List
import Prelude

main :: IO()
main  = do
      s <- readLn
      imprimeSudoku (solve s 0 0 1)



splitString :: String -> String
splitString (x:xs)
      | null xs = []
      | x == fecha && fechavir == head xs = splitString (tail xs)
      | x == fecha  = splitString xs
      | otherwise = x:splitString xs
      where (fecha,fechavir) = (']', ',')

transformInt :: [String] -> [Int]
transformInt  [] = []
transformInt ("":xs) = 0:transformInt xs
transformInt (x:xs) = (read x::Int)  : transformInt xs

splitOn2 :: String -> [[String]]
splitOn2 xs = map (splitOn ",") (filter (not . null) (splitOn "[" $ splitString xs))

pegaCampo :: String -> [[Int]]
pegaCampo xs = map transformInt (splitOn2 xs)

retornaLinha :: [[Int]] -> Int -> [Int]
retornaLinha s l = s !! l

retornaColuna :: [[Int]] -> Int -> [Int]
retornaColuna s c = transpose s!!c

retornaValor :: [[Int]] -> Int -> Int -> Int
retornaValor s l c = (s !! l) !! c

colunaVazia :: [[Int]] -> Int -> Int -> Bool
colunaVazia s c j = j `elem` [1..9] && j `notElem` retornaColuna s c

linhaVazia :: [[Int]] -> Int -> Int -> Bool
linhaVazia s l j = j `elem` [1..9] && j `notElem` retornaLinha s l

quadradoVazio :: [[Int]] -> Int -> Int -> Int -> Bool
quadradoVazio s l c j = j `notElem` retornaQuadrado s l c


retornaQuadrado :: [[Int]] -> Int -> Int -> [Int]
retornaQuadrado s l c
    | l `elem` [0..2] && c `elem` [0..2] = map (head s !!) [0..2] ++ map (s !! 1 !!) [0..2] ++ map (s !! 2 !!) [0..2]
    | l `elem` [3..5] && c `elem` [0..2] = map (s !! 3 !!) [0..2] ++ map (s !! 4 !!) [0..2] ++ map (s !! 5 !!) [0..2]
    | l `elem` [6..8] && c `elem` [0..2] = map (s !! 6 !!) [0..2] ++ map (s !! 7 !!) [0..2] ++ map (s !! 8 !!) [0..2]
    | l `elem` [0..2] && c `elem` [3..5] = map (head s !!) [3..5] ++ map (s !! 1 !!) [3..5] ++ map (s !! 2 !!) [3..5]
    | l `elem` [3..5] && c `elem` [3..5] = map (s !! 3 !!) [3..5] ++ map (s !! 4 !!) [3..5] ++ map (s !! 5 !!) [3..5]
    | l `elem` [6..8] && c `elem` [3..5] = map (s !! 6 !!) [3..5] ++ map (s !! 7 !!) [3..5] ++ map (s !! 8 !!) [3..5]
    | l `elem` [0..2] && c `elem` [6..8] = map (head s !!) [6..8] ++ map (s !! 1 !!) [6..8] ++ map (s !! 2 !!) [6..8]
    | l `elem` [3..5] && c `elem` [6..8] = map (s !! 3 !!) [6..8] ++ map (s !! 4 !!) [6..8] ++ map (s !! 5 !!) [6..8]
    | l `elem` [6..8] && c `elem` [6..8] = map (s !! 6 !!) [6..8] ++ map (s !! 7 !!) [6..8] ++ map (s !! 8 !!) [6..8]
    | otherwise = []

jogadaValida :: [[Int]] -> Int -> Int -> Int -> Bool
jogadaValida s l c j = linhaVazia s l j && colunaVazia s c j && quadradoVazio s l c j


--Trecho desenvolvido Com ajuda do aluno Matheus Endo
fazJogada :: [[Int]] -> Int -> Int -> Int -> [[Int]]
fazJogada sudoku linha col valor =
  take linha sudoku ++ [take col (sudoku !! linha) ++ [valor] ++ drop (col + 1) (sudoku !! linha)] ++ drop (linha + 1) sudoku


solve :: [[Int]] -> Int -> Int -> Int -> [[Int]]
solve s l c j
    | j `notElem` [1..9] = [[]]
    | c `notElem` [0..8] = solve s (l+1) c 1
    | l `notElem` [0..8] = s
    | retornaValor s l c /= 0 = solve s l (c+1) 1
    | not $ jogadaValida s l c j = solve s l c (j+1)
    | otherwise = solucao' s l c j

solucao' :: [[Int]] -> Int -> Int -> Int -> [[Int]]
solucao' s l c j = let temp = solve (fazJogada s l c j) l c 1 in
                    if temp == [[]] then solve s l c (j + 1) else temp

imprimeSudoku :: [[Int]] -> IO()
imprimeSudoku [] = return()
imprimeSudoku (x:xs) = do imprimeLinha x
                          imprimeSudoku xs

imprimeLinha :: [Int] -> IO()
imprimeLinha l= putStrLn $ unwords . map show $ l
