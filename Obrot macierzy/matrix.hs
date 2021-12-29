import Data.List
-- definicja typów
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
type Vector = [Int]
type Matrix = [[Int]]

z :: Vector
z = [1,2,3]
m :: Matrix
m = [[1,2,3],
     [4,5,6]]


-- odczytywanie liczby wektorow i kolumn

numRows :: Matrix -> Int
numRows = length
numColumns :: Matrix -> Int
numColumns =  length . head

c :: Matrix
c = [[1,2,3],
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

-- odejmowanie dwoch wektorow
vectorSubtraction :: Vector -> Vector -> Vector
vectorSubtraction = zipWith' (-)
-- Sumowanie dwoch Macierzy
matrixSum :: Matrix -> Matrix -> Matrix
matrixSum = zipWith' vectorSum

--Iloczyn Skalarny wektorow
dotProduct :: Vector -> Vector -> Int
dotProduct v w = sum (zipWith' (*) v w)

-- mnozenie macierzy
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct row) (transpose n) | row <- m  ] -- map wyciaga wiersz, transpose zamienia wiersze na kolumny i oblicza iloczyn skalarny


v :: Vector
u :: Vector
x :: Vector
v = [64, 64,64,64,62,60,71,62,62,62,62,60] 
u = [60,62,63,65,67,68,70,72,74,76,78,80] 
x = [48,50,52,54,56,58,60,62,64,66,68,70] 
