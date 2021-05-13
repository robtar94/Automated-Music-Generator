import Data.List
-- definicja typów
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
type Vector = [Int]
type Matrix = [[Int]]

v :: Vector
v = [1,2,3]
m :: Matrix
m = [[1,2,3],
     [4,5,6]]


-- odczytywanie liczby wektorow i kolumn

numRows :: Matrix -> Int
numRows = length
numColumns :: Matrix -> Int
numColumns =  length . head

x :: Matrix
x = [[1,2,3],
      [4,5,6],
      [7,3,2]]

-- generowanie wektora (np do rytmu - ciag 0 lub 1 lub innych wartosci) 
autoVector :: Int -> Vector
autoVector n = replicate n 1

-- mnozenie wektora przez skalar

multVector :: Int -> Vector -> Vector
multVector n vector = [n * x | x <- vector]

w = multVector 3 v -- mnozenie wektora przez 3

-- mnozenie macierzy przez skalar
multMatrix :: Int -> Matrix -> Matrix
multMatrix n m = [ multVector n row | row <- m]
-- przyklad
m2 :: Matrix
m2 = multMatrix 3 m -- mnozenie macierzy przez 3

--ZipWith'
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- sumowanie dwoch wektorow
vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith' (+)
-- Sumowanie dwoch Macierzy
matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith' vectorSum

--Iloczyn Skalarny wektorow
dotProduct :: Vector -> Vector -> Int
dotProduct v w = sum (zipWith' (*) v w)

-- mnozenie macierzy
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct row) (transpose n) | row <- m  ] -- map wyciaga wiersz, transpose zamienia wiersze na kolumny i oblicza iloczyn skalarny



y :: Matrix
y = [[1,2,3],[4,5,6],[1,2,1]]

z :: Matrix
z = [[4,5,6], [1,2,1],[3,1,1]] 

