import Euterpea
import qualified Data.List as List
-- definicja typów
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
type Vector = [Float]
type Matrix = [[Float]]

-- konwersja z Pitch na AbsPitch
toAbsPitch :: [Pitch] -> [AbsPitch]
toAbsPitch = map absPitch  

-- konwersja z AbsPitch na Pitch
toPitch :: [AbsPitch] -> [Pitch]
toPitch  = map pitch 



--dodanie nut

-- Definicja nuty (rownorzedny zapis)

--Prim (Note qn (C, 4))
--note qn (C, 4)
--c 4 qn

pcToSN :: PitchClass -> Music Pitch
pcToSN pc = note sn (pc, 4)

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

pcToHN :: PitchClass -> Music Pitch
pcToHN pc = note hn (pc, 4)

--ZipWith'
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

toInt :: Float -> AbsPitch 
toInt x = round x

mod12 :: [AbsPitch] -> [AbsPitch]
mod12 x = map (`mod` 12) x 


--Iloczyn Skalarny wektorow
dotProduct :: Vector -> Vector -> Float
dotProduct v w = sum (zipWith' (*) v w)

-- mnozenie macierzy
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct row) (List.transpose n) | row <- m]

-- konwersja matrycy na wektor

myconcat :: Matrix -> Vector
myconcat xss = [x | xs <- xss, x <- xs]



takt1 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, gs 4 sn,  e 4 sn]
takt2 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, b 4 sn, gs 4 sn]
takt3 = line [a 4 sn, a 4 sn, c 4 qn, d 4 qn, e 4 hn]


utwor :: [AbsPitch]
utwor = [69,69,69,68,64,69,69,69,71,68, 69, 69, 60]   -- mazurek 13 dzwiekow


w :: Matrix
w = [[69,69,69]]
u :: Matrix
u = [[68, 64, 69]]
v :: Matrix
v = [[69,69, 71]]

kat = 5
-- macierze obrotu kata 5 stopni
z :: Matrix
z = [[cos kat, sin kat, 0], [-sin kat, cos kat, 0], [0,0,1]]
y :: Matrix
y = [[cos kat, 0, -sin kat], [0,1,0], [sin kat, 0, cos kat]]
x :: Matrix
x = [[1,0,0], [0, cos kat, sin kat], [0, -sin kat, cos kat]]


-- obliczanie macierzy obrotu
matrycaZ :: Matrix
matrycaZ = matrixProduct w z

-- konwersja macierzy na liste []
wektor :: Vector
wektor = myconcat matrycaZ
-- konwersja z Double na Int
wektorInt :: [AbsPitch]
wektorInt =  map toInt wektor

 -- proba zmapowania wartosci ujemnych na dodatnie. Nie umiem tego zaaplikowac do listy AbsPitch
pcEq :: PitchClass  -> AbsPitch  --pcToInt Cf =
pcEq x  = pcToInt x `mod` 12  








