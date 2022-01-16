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
wzorzec1 = [69,69,69,68,64,69,69,69,71,68,69,69] -- v
wzorzec2 = [69,65,67,65,69,65,69,65,67,71,65,67] -- u
z = [67,64,60,67,64,60,67,64,60,67,64,60,67,64]
-- konwersja na float 
v = floatList wzorzec1 
u = floatList wzorzec2
--x = floatList z
x = [81.231476,106.587074,32.426453,90.44081,94.751656,72.08647,59.459023,99.026505,68.98509,33.308205,85.71005,96.7362,47.614586,56.77819]
-- miejsce na wektor x, ktory trzeba wkleic po wykonaniu funkcji test z pliku generuj.hs 

y = odejmij v u
y_x = dot y x -- iloczyn skalarny y|x

w_0 :: Float
w_0 = (dot v v   - dot u u )  / 2 -- w_0

klasyfikacja :: Float -> Int 
klasyfikacja y_x
 | y_x > w_0 = 1 -- klasa C1 ludowa
 | y_x < w_0 = -1 --klasa C2 jazzowa
