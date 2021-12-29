import Euterpea
-- Konwersja listy int na float
floatList :: [AbsPitch] -> [Float]
floatList [] = []
floatList (n:ns)  = (x) : floatList(ns)
    where x = fromIntegral n :: Float

    -- Iloczyn skalarny
dot ::  [Float] ->  [Float] -> Float
dot x y = sum $ zipWith (*) x y

    
-- odejmowanie dwoch wektorow
odejmij :: [Float] -> [Float] -> [Float]
odejmij [] _ = []
odejmij _ [] = []
odejmij (x:xs) (y:ys) = x - y : (odejmij xs ys)

-- przebieg (sposob 1)
-- wektory bazowe

wzorzec1 :: [AbsPitch]
wzorzec2 :: [AbsPitch]
wzorzec1 = [1,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1] -- v
wzorzec2 = [1,0,1,1,0,0,1,1,1,1,1,1,0,0,1,0,1,1,0,0,1] -- u

-- konwersja na float 
v = floatList wzorzec1 
u = floatList wzorzec2
-- miejsce na wektor x, ktory trzeba wkleic po wykonaniu funkcji randomBin test z pliku generuj.hs 21 dzwiekow
x = [1,1,1,0,1,0,1,0,0,1,1,0,0,1,1,1,1,0,1,1,0]

y = odejmij v u
y_x = dot y x

w_0 :: Float

w_0 = (dot v v   - dot u v )  / 2

klasyfikacja :: Float -> Int 
klasyfikacja y_x
 | y_x > w_0 = 1 -- klasa C1
 | y_x < w_0 = -1 --klasa C2
