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
toPitch = map pitch

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

toInt :: Float -> Int
toInt = round


--Iloczyn Skalarny wektorow
dotProduct :: Vector -> Vector -> Float
dotProduct v w = sum (zipWith' (*) v w)

-- mnozenie macierzy
matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct row) (List.transpose n) | row <- m]


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
-- macierze obrotu kata 90 stopni
z :: Matrix
z = [[cos 90, sin kat, 0], [-sin kat, cos kat, 0], [0,0,1]]
y :: Matrix
y = [[cos kat, 0, -sin kat], [0,1,0], [sin kat, 0, cos kat]]
x :: Matrix
x = [[1,0,0], [0, cos kat, sin kat], [0, -sin kat, cos kat]]

-- mnozenie przez macierz obrotu

-- os z
w1 :: Matrix
w1 = matrixProduct w  z
u1 = matrixProduct u z
v1 = matrixProduct v z

w2 :: [AbsPitch]
w2 = [-93,31,69]
u2 :: [AbsPitch]
u2 = [-88, 32, 69]
v2 :: [AbsPitch]
v2 = [93, 30, 71]

-- po obrocie wzgledem osi z
w3 :: Music Pitch
w3 = line [ds 9 qn, g 1 qn, a 4 qn]
u3 :: Music Pitch
u3 = line [gs 9 qn, gs 1 qn, a 4 qn]
v3 = line [a 5 qn, fs 1 qn, b 4 qn]

zAxis :: Music Pitch
zAxis = w3 :+: u3 :+: v3

-- os Y obrot
wy :: Matrix
wy = matrixProduct w y
uy :: Matrix
uy = matrixProduct u y
vy :: Matrix
vy = matrixProduct v y




wy1 :: [AbsPitch]
wy1 = [30,69,93]
wy2 :: Music Pitch
wy2 = line [fs 1 qn, a 4 qn, a 6 qn]

uy1 :: [AbsPitch]
uy1 = [31, 64, -92]
uy2 :: Music Pitch
uy2 = line [g 1 qn, e 4 qn, e 9 qn]

vy1 :: [AbsPitch]

vy1 = [33, 69, -93]

vy2 :: Music Pitch
vy2 = line [a 1 qn, a 4 qn, ds 9 qn]

-- Oś Y
yAxis = wy2 :+: uy2 :+: vy2

-- os X obrot


wx :: Matrix
wx = matrixProduct w x
ux :: Matrix
ux = matrixProduct u x
vx :: Matrix
vx = matrixProduct v x




wx1 :: [AbsPitch]
wx1 = [69,-93,31]
wx2 :: Music Pitch
wx2 = line [a 4 qn, ds 9 qn, g 1 qn]

ux1 :: [AbsPitch]
ux1 = [68, -90, -26]
ux2 :: Music Pitch
ux2 = line [gs 4 qn, fs 9 qn, as 4 qn]

vx1 :: [AbsPitch]

vx1 = [69, 94, 30]

vx2 :: Music Pitch
vx2 = line [a 4 qn, as 6 qn, fs 1 qn]


-- Oś X

xAxis = wx2 :+: ux2 :+: vx2




org = line [a 4 qn, a 4 qn, a 4 qn, gs 4 qn, e 4 qn , a 4 qn, a 4 qn, a 4 qn, b 4 qn]

zmieniony = zAxis :+: yAxis :+: xAxis


