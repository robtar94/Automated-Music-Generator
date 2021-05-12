import  Data.List 

-- I sposob

-- const x _ = x

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs -- n liczba wymiarow

macierz2 = [7,9,3,2,4,1] 

macierz3 = rotate 4 macierz2 -- obrot macierzy 2 w 4 wymiarach



-- II sposob: funkcje wbudowane
-- -90
rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse.transpose

-- +90
rotateRight :: [[a]] -> [[a]]
rotateRight = transpose.reverse

--przykladowa macierz
macierz = [[1,2,3],[0,4,5],[0,0,6]]

lewo = rotateLeft macierz
-- wynik: [[3,5,6],[2,4,0],[1,0,0]]
prawo = rotateRight macierz

-- przyklad


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





-- III sposob obrot o kat 90 stopni


matrix :: [[a]] -> [[a]]

--- 1. mnozenie macierzy przez wektor
--- 2. mnozenie macierzy przez macierz