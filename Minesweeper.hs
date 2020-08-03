module Main where

import Data.Char
import System.IO
import System.Random

type GBoard = [[Char]]
type MBoard = [[Bool]]

gBoard :: GBoard
gBoard = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]

mBoard :: MBoard
mBoard = [[False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, True , False, False, False, False],
          [False, False, False, False, False, True, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False],
          [False, False, False, False, False, False, False, False, False]]

gArr :: Int -> [t] -> t
gArr i arr = head $ drop i arr 

uArr :: Int -> a -> [a] -> [a]
uArr i x xs = back ++ [x] ++ front
    where 
        back = take i xs 
        front = drop (i + 1) xs

gPos :: Int -> Int -> [[a]] -> a
gPos i j m = gArr j $ gArr i m

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos i j x m = uArr i (uArr j x (gArr i m)) m    

isMine :: Int -> Int -> MBoard -> Bool
isMine i j = gPos i j

isValidPos :: Int -> Int -> Int -> Bool
isValidPos size i j 
    | i >= size || j >= size || i < 0 || j < 0 = False
    | otherwise = True 

validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves size i j = filter (uncurry (isValidPos size)) adjascent
    where adjascent = foldl (++) [] $ map (\x -> map (\y -> (x + i, y + j))[-1..1]) [-1..1]  

cMinas :: Int -> Int -> MBoard -> Int
cMinas i j b = sum . map ((\b -> if b then 1 else 0) . (\(x, y) -> isMine x y b)) $ moves 
    where 
        size = length b
        moves = validMoves size i j 

--- abreJogada: é a função principal do jogo!!
--- recebe uma posição a ser aberta (linha e coluna), o mapa de minas e o tabuleiro do jogo. Devolve como
--  resposta o tabuleiro do jogo modificado com essa jogada.
--- Essa função é recursiva, pois no caso da entrada ser uma posição sem minas adjacentes, o algoritmo deve
--- seguir abrindo todas as posições adjacentes até que se encontre posições adjacentes à minas.
--- Vamos analisar os casos:
--- - Se a posição a ser aberta é uma mina, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta já foi aberta, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta é adjacente a uma ou mais minas, devolver o tabuleiro modificado com o número de
--- minas adjacentes na posição aberta
--- - Se a posição a ser aberta não possui minas adjacentes, abrimos ela com zero (0) e recursivamente abrimos
--- as outras posições adjacentes a ela

abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada i j m g  
    | gPos i j m        = g
    | gPos i j g /= '-' = g
    | nMinas /= 0       = uPos i j (intToDigit nMinas) g
    | nMinas == 0       = let newG = uPos i j '0' g in foldMoves newG 
        where 
            nMinas = cMinas i j m
            moves = validMoves size i j
            size = length m 
            foldMoves g = foldr (\(x, y) prev -> abreJogada x y m prev) g moves   

--- abreTabuleiro: recebe o mapa de Minas e o tabuleiro do jogo, e abre todo o tabuleiro do jogo, mostrando
--- onde estão as minas e os números nas posições adjecentes às minas. Essa função é usada para mostrar
--- todo o tabuleiro no caso de vitória ou derrota

abreTabuleiro :: MBoard -> GBoard -> GBoard
abreTabuleiro m g = map (\x -> map (\y -> f x y) [1..size - 1]) [1..size - 1]
    where
        size = length m
        f x y 
            | gPos x y m = '*'
            | otherwise = intToDigit $ cMinas x y m 


--  -- contaFechadas: Recebe um GBoard e conta quantas posições fechadas existem no tabuleiro (posições com '-')
mapMatrix :: (a -> b) -> [[a]]  -> [[b]]
mapMatrix f m = map (\xs -> map (\y -> f y) xs) m

contaCond :: (a -> Bool) -> [[a]] -> Int
contaCond cond m = sum binary
    where
        flat = foldr (++) [] $ mapMatrix cond m 
        binary = map (\b -> if b then 1 else 0) flat

contaFechadas :: GBoard -> Int
contaFechadas = contaCond (== '-')

contaMinas :: MBoard -> Int
contaMinas = contaCond (id)

endGame :: MBoard -> GBoard -> Bool
endGame b g = (contaMinas b) == (contaFechadas g)

intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala x xs = head xs:x:intercala x (tail xs)

printBoard :: GBoard -> String
printBoard g = header ++ foldr (\(cs, col) prev -> '\n':show col ++ ": " ++ (intercala ' ' cs) ++ prev) "" (zip g [0..size - 1]) ++ "\n"
    where 
        header = "  " ++ (foldr (\c prev-> ' ':c ++ prev) "" $ map show [0..size - 1]) ++ "\n"
        size = length g

-- geraLista: recebe um inteiro n, um valor v, e gera uma lista contendo n vezes o valor v

geraLista :: Int -> a -> [a]
geraLista n = take n . repeat

-- geraTabuleiro: recebe o tamanho do tabuleiro e gera um tabuleiro  novo, todo fechado (todas as posições
-- contém '-'). A função geraLista deve ser usada na implementação

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro size = map (\x -> geraLista size '-') [0..size - 1]

-- geraMapaDeMinasZerado: recebe o tamanho do tabuleiro e gera um mapa de minas zerado, com todas as posições
-- contendo False. Usar geraLista na implementação

geraMapaDeMinasZerado :: Int -> MBoard
geraMapaDeMinasZerado size = map (\x -> geraLista size False) [0..size - 1]

main :: IO ()
main = do
   putStr "Digite o tamanho do tabuleiro: "
   size <- getLine
   mb <- genMinesBoard (read size)
   gameLoop mb (geraNovoTabuleiro (read size)) 

gameLoop :: MBoard -> GBoard -> IO ()
gameLoop mb gb = do
   putStr (printBoard gb)
   putStr "Digite uma linha: "
   linha <- getLine
   putStr "Digite uma coluna: "
   coluna <- getLine
   if (isMine (read linha) (read coluna) mb)
      then do
            putStr "VOCE PERDEU!\n"
            putStr $ printBoard $ abreTabuleiro mb gb
            putStr "TENTE NOVAMENTE!\n"
      else do
            let newGB = (abreJogada (read linha) (read coluna) mb gb)
            if (endGame mb newGB)
                 then do
                     putStr "VOCE VENCEU!!!!!!!!\n"
                     putStr $ printBoard $ abreTabuleiro mb newGB
                     putStr "PARABENS!!!!!!!!!!!\n"
                 else
                     gameLoop mb newGB 


genMinesBoard :: Int -> IO MBoard
genMinesBoard size = do
        board <- addMines (round   ((fromIntegral (size *size)) * 0.15)) size (geraMapaDeMinasZerado size) 
        return board

addMines :: Int -> Int -> MBoard -> IO MBoard
addMines 0 size b = return b
addMines n size b = do
                l <- randomRIO (0,(size-1))
                c <- randomRIO (0,(size-1))
                case isMine l c b of
                      True -> addMines n size b
                      False -> addMines (n-1) size (uPos l c True b)

