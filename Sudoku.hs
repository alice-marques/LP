--Matheus Eiji Endo - 15/0018169
--Johannes Peter Schulte - 150132662
import Prelude 
import Data.List 


main = do 
    putStrLn "Entrada:"
    sudoku <- pegaEntrada
    putStrLn "\nSolução:"
    carregaSudoku (solucao sudoku 0 0 1)

--checa se e valido colocar o numero na posicao linha,coluna. Utiliza outras funcoes
valido :: [[Int]] -> Int -> Int -> Int -> Bool
valido sudoku linha coluna valor = (linhaValida sudoku linha valor) && (colunaValida sudoku coluna valor) && (quadradoValido sudoku linha coluna valor)

--checa se existe outro elemento igual ao valor no quadrado
quadradoValido :: [[Int]] -> Int -> Int -> Int -> Bool
quadradoValido sudoku linha coluna valor = not(elem valor (pegaQuadrado sudoku linha coluna)) && (elem valor [1,2..9])

--checa se existe outro elemento igual ao valor na linha
linhaValida :: [[Int]] -> Int -> Int -> Bool
linhaValida sudoku linha valor = not(elem valor (pegaLinha sudoku linha)) && (elem valor [1,2..9])

--checa se eiste outro elemento igual ao valor na coluna
colunaValida :: [[Int]] -> Int -> Int -> Bool
colunaValida sudoku col valor = not(elem valor (pegaColuna sudoku col)) && (elem valor [1,2..9])

--dado linha e coluna retorna a qual quadrado pertence o valor
qualQuadrado :: [[Int]] -> Int -> Int -> Int
qualQuadrado sudoku linha coluna
    | ((elem coluna [0,1,2]) && (elem linha [0,1,2])) = 1
    | ((elem coluna [3,4,5]) && (elem linha [0,1,2])) = 2
    | ((elem coluna [6,7,8]) && (elem linha [0,1,2])) = 3
    | ((elem coluna [0,1,2]) && (elem linha [3,4,5])) = 4
    | ((elem coluna [3,4,5]) && (elem linha [3,4,5])) = 5
    | ((elem coluna [6,7,8]) && (elem linha [3,4,5])) = 6
    | ((elem coluna [0,1,2]) && (elem linha [6,7,8])) = 7
    | ((elem coluna [3,4,5]) && (elem linha [6,7,8])) = 8
    | ((elem coluna [6,7,8]) && (elem linha [6,7,8])) = 9
    | otherwise = 0
    
----retorna uma lista contendo os elementos do quadrado
pegaQuadrado :: [[Int]] -> Int -> Int -> [Int]
pegaQuadrado sudoku linha coluna 
    | ((qualQuadrado sudoku linha coluna )==1) = (take 3 (sudoku!!0) ++ take 3(sudoku!!1) ++ take 3(sudoku!!2))
    | ((qualQuadrado sudoku linha coluna )==2) = (take 3 (drop 3(sudoku!!0)) ++ take 3(drop 3(sudoku!!1)) ++ take 3(drop 3(sudoku!!2)))
    | ((qualQuadrado sudoku linha coluna )==3) = (take 3 (drop 6(sudoku!!0)) ++ take 3(drop 6(sudoku!!1)) ++ take 3(drop 6(sudoku!!2)))
    | ((qualQuadrado sudoku linha coluna )==4) = (take 3 (sudoku!!3) ++ take 3(sudoku!!4) ++ take 3(sudoku!!5))
    | ((qualQuadrado sudoku linha coluna )==5) = (take 3 (drop 3(sudoku!!3)) ++ take 3(drop 3(sudoku!!4)) ++ take 3(drop 3(sudoku!!5)))
    | ((qualQuadrado sudoku linha coluna )==6) = (take 3 (drop 6(sudoku!!3)) ++ take 3(drop 6(sudoku!!4)) ++ take 3(drop 6(sudoku!!5)))
    | ((qualQuadrado sudoku linha coluna )==7) = (take 3 (sudoku!!6) ++ take 3(sudoku!!7) ++ take 3(sudoku!!7))
    | ((qualQuadrado sudoku linha coluna )==8) = (take 3 (drop 3(sudoku!!6)) ++ take 3(drop 3(sudoku!!7)) ++ take 3(drop 3(sudoku!!8)))
    | ((qualQuadrado sudoku linha coluna )==9) = (take 3 (drop 6(sudoku!!6)) ++ take 3(drop 6(sudoku!!7)) ++ take 3(drop 6(sudoku!!8)))
    | ((qualQuadrado sudoku linha coluna )==0) = []

--retorna uma lista contendo os elementos da linha
pegaLinha :: [[Int]] -> Int -> [Int]
pegaLinha sudoku linha = sudoku!!linha

--retorna uma lista contendo os elementos da coluna
pegaColuna :: [[Int]] -> Int -> [Int]
pegaColuna sudoku col = (transpose sudoku)!!col 
  
--retorna o valor de dado ponto
pegaValor :: [[Int]] -> Int -> Int -> Int
pegaValor sudoku linha col = (sudoku!!linha) !!col

--coloca o valor em determinado ponto
colocaValor :: [[Int]] -> Int -> Int -> Int -> [[Int]]
colocaValor sudoku linha col valor =
  take linha sudoku ++ [take col (sudoku !! linha) ++ [valor] ++ drop (col + 1) (sudoku !! linha)] ++ drop (linha + 1) sudoku
  

--solucao, a ideia e 'andar' pelas linhas do sudoku e checar se existe 0, a partir dai chama solucao' , que utiliza solucao 
--para ver se e possivel chegar a resposta a partir daquele ponto, se nao retorna vazio.
--utiliza guards para checar se os parametros x e y nao 'saem' do sudoku e se o valor nao excede 9
solucao :: [[Int]] -> Int -> Int -> Int-> [[Int]]
solucao sudoku linha coluna valor
    | (valor > 9) = [[]]
    | (linha > 8) = sudoku
    | (coluna > 8) = (solucao sudoku (linha+1) 0 1)
    | ((pegaValor sudoku linha coluna) > 0) = (solucao sudoku linha (coluna+1) 1)
    | (valido sudoku linha coluna valor)==False = solucao sudoku linha coluna (valor + 1)
    | otherwise = solucao' sudoku linha coluna valor
        
solucao' :: [[Int]] -> Int -> Int -> Int-> [[Int]]
solucao' sudoku linha coluna valor = (let temp=(solucao (colocaValor sudoku linha coluna valor) linha coluna 1) in
    if temp== [[]] then (solucao sudoku linha coluna (valor+1)) else temp)


pegaEntrada :: IO [[Int]]
x = readLn
pegaEntrada = x

--carrega o sudoku na tela, utiliza recursao dividindo a lista de listas em head e tail ate ate que esteja vazia
carregaSudoku :: [[Int]] -> IO()
carregaSudoku [] = return ()
carregaSudoku (x:xs) = do carregaLinha x
                          carregaSudoku xs

--utilizada em carregaSudoku para carregar cada linha na tela
carregaLinha :: [Int] -> IO ()
carregaLinha linha = do
    putStrLn $ unwords . map show $ linha
           
  

