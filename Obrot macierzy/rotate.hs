import qualified Data.List as List
import Euterpea


-- konwersja

konwersja :: [Pitch] -> [AbsPitch]
konwersja [] = []
konwersja (x:xs) =  absPitch x : konwersja xs 


-- funkcje wbudowane
-- -90
rotateLeft :: [[a]] -> [[a]]
rotateLeft = List.reverse.List.transpose

-- +90
rotateRight :: [[a]] -> [[a]]
rotateRight = List.transpose.List.reverse

--przykladowa macierz
macierz = [[1,2,3],[0,4,5],[0,0,6]]

lewo = rotateLeft macierz
-- wynik: [[3,5,6],[2,4,0],[1,0,0]]
prawo = rotateRight macierz

-- 2 sposob

-- const x _ = x

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

macierz2 = [7,9,3,2,4,1]

-- obrot o 180 stopni
x = [[1,2,3], [4,5,6], [7,8,9]]

-- +90
x1 = rotateRight x 

-- +180
x2 = rotateRight x1


-- -90
y1 = rotateLeft x 

-- -180
y2 = rotateLeft x1





