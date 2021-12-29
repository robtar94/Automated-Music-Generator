import Euterpea
-- Konwersja listy int na float
floatList :: [AbsPitch] -> [Float]
floatList [] = []
floatList (n:ns)  = (x) : floatList(ns)
    where x = fromIntegral n :: Float





    -- funkcje ogolne
-- odejmowanie dwoch wektorow
odejmij :: [Float] -> [Float] -> [Float]
odejmij [] _ = []
odejmij _ [] = []
odejmij (x:xs) (y:ys) = x - y : (odejmij xs ys)

-- Iloczyn skalarny
dot ::  [Float] ->  [Float] -> Float
dot x y = sum $ zipWith (*) x y

-- przebieg (sposob 1)
-- wektory bazowe

wzorzec1 :: [AbsPitch]
wzorzec2 :: [AbsPitch]
wzorzec1 = [64, 64,64,64,62,60,71,62,62,62,62,60] -- v
wzorzec2 = [60,62,63,65,67,68,70,72,74,76,78,80] -- u

-- konwersja na float 
v = floatList wzorzec1 
u = floatList wzorzec2
-- miejsce na wektor x, ktory trzeba wkleic po wykonaniu funkcji test z pliku generuj.hs 
x = [41.915123,92.65413,91.944275,93.80305,47.968163,44.286293,28.69761,103.59771,20.613039,102.827866,64.188614,85.32071]
y = odejmij v u
y_x = dot y x -- iloczyn skalarny y|x

w_0 :: Float
w_0 = (dot v v   - dot u v )  / 2 -- w_0

klasyfikacja :: Float -> Int 
klasyfikacja y_x
 | y_x > w_0 = 1 -- klasa C1
 | y_x < w_0 = -1 --klasa C2
