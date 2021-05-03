import System.Random
import Test.QuickCheck (generate, elements)

-- 1 utwor: http://oskarkolberg.pl/en-US;q=0.7/MusicDb/Details/cafbb535-ed69-4b99-9002-f65fe40ed8fe
-- "Od Pilicy i Wolbroma"
import Euterpea
takt1 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, gs 4 sn,  e 4 sn]
takt2 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, b 4 sn, gs 4 sn]
takt3 = line [a 4 sn, a 4 sn, c 4 qn, d 4 qn, e 4 hn]
takt4 = line [e 4 sn, e 4 sn, e 4 sn, e 4 sn,  d 4 sn, c 4 sn]
takt5 = line [b 4 sn,  d 4 sn,  d 4 sn, d 4 sn, d 4 sn, c 4 sn]
takt6 = line [a 4 sn, b 4 sn, c 4 qn, rest qn, c 4 qn, rest qn, a 4 hn]

utwor1 = takt1 :+: takt2 :+: takt3 :+: takt4 :+: takt5 :+: takt6

jazz = line [a 5 en, f 5 en, g 5 en, f 5 en, a 5 qn, f 5 qn, a 5 qn, f 5 qn, g 5 qn, a 5 qn, f 5 qn, g 5 qn, f 5 qn]


-- konwersja na AbsolutePitch:

linia1 = [69,69,69,68,64]
linia2 = [69,69,69,71,68]
linia3 = [69, 69, 60,62, 64]
linia4 = [64, 64,64,64,62,60]
linia5 = [71,62,62,62,62,60]
linia6 = [69,71,60,60,69]

v = [81, 77, 79, 77, 81, 77, 81, 77, 79, 81, 77, 79, 77] 
u = [69,69,69,68,64,69,69,69,71,68, 69, 69, 60] 


x = [66,68..138] 


w_0 :: Float
w_0 = (dot v v   - dot u v )  / 2

y = odejmij v u

y_x = dot y x





-- Funkcje


-- odejmowanie dwoch wektorow
odejmij :: [Float] -> [Float] -> [Float]
odejmij [] _ = []
odejmij _ [] = []
odejmij (x:xs) (y:ys) = x - y : (odejmij xs ys)


-- Iloczyn skalarny
dot :: [Float] -> [Float] -> Float
dot x y = sum $ zipWith (*) x y

klasyfikacja:: Float -> Int 
klasyfikacja y_x
    | y_x > w_0 = 1 -- klasa C1
    | y_x < w_0 = -1 --klasa C2


