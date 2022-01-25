import Euterpea
import Data.List as L
-- definicja typów
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
type Vector = [Double]
type Matrix = [[Double]]

-- macierz obrotu
mx p = [[1,0,0], [0, cos p, sin p], [0, -sin p, cos p]] -- p = phi
my p = [[cos p, 0, - sin p], [0, 1, 0], [sin p, 0, cos p]]
mz p = [[cos p, sin p, 0], [-sin p, cos p, 0], [0, 0, 1]]


-- UTWOR BAZOWY
linia1 :: [PitchClass]
linia1 = [C, E, G, B, D, F, A, C, E, G, B, C] -- sekunda wielka
linia2 = [C, F, A, D, G, C, F, B, E, A, D, G] -- tercja wielka
linia3 = [C, A, F, D, B, G, E, C, A, F, D, B] -- kwinta czysta
linia4 = [C, G, E, B, G, D, B, F, D, A, F, C] -- tryton

-- pomocniczo
part1 = map pcToHN (linia1)
part2 =  map pcToQN (linia2)
part3 = map pcToHN (linia3)
part4 =  map pcToQN(linia4)
part1_music = line $ part1
part2_music = line $ part2
part3_music = line $ part3
part4_music = line $ part4


-- caly utwor

utwor = part1_music :+: part2_music :+: part3_music :+: part4_music

-- konwersja linii melodycznych na liczby

part1_int = map pcToInt12 (linia1)
part2_int = map pcToInt12 (linia2)
part3_int = map pcToInt12 (linia3)
part4_int = map pcToInt12 (linia4)

-- macierze do obrotu 3D

mXY = mmult (mx 20) (my 20) 
mXYZ = mmult (mXY) (mz 20) -- cala macierz obrotu o kat phi

-- wektory
v1 :: Vector
v1 = [0,4,7,11,2,5,9,0,4,7,11,0]
v2 :: Vector
v2 =[0,5,9,2,7,0,5,11,4,9,2,7]
v3 :: Vector
v3 = [0,9,5,2,11,7,4,0,9,5,2,11]
v4 :: Vector
v4 =[0,7,4,11,7,2,11,5,2,9,5,0]

w1 :: Matrix
w1 = [[0,4,7,11,2,5,9,0,4,7,11,0]]
w2 :: Matrix
w2 =[[0,5,9,2,7,0,5,11,4,9,2,7]]
w3 :: Matrix
w3 = [[0,9,5,2,11,7,4,0,9,5,2,11]]
w4 :: Matrix
w4 =[[0,7,4,11,7,2,11,5,2,9,5,0]]

--obracanie wektorow
wektor1 = do
    toInt(concat(mmult (w1) mXYZ))
    

wektor2 = do
    toInt(concat(mmult (w2) mXYZ))
    
wektor3 = do
    toInt(concat(mmult (w3) mXYZ))
    

wektor4 = do
    toInt(concat(mmult (w4) mXYZ))
    


-- 1. Obrot o 5 stopni
rotate1 = [8,1,1,10,1,1,9,5,2,7,3,2]
-- o 10 stopni
rotate2 = [3,3,7,4,4,9,7,1,8,5,1,6]
-- o 15 stopni
rotate3 = [6,5,2,8,6,3,6,9,2,4,7,1]
-- o 20 stopni 
rotate4 = [7,3,3,9,4,3,5,8,4,4,6,3]

-- Pitche po obrocie
r1_pitch = [(Gs,4),(Cs,4),(Cs,4),(As,4),(Cs,4),(Cs,4),(A,4),(F,4),(D,4),(G,4),(Ds,4),(D,4)]
r2_pitch = [(Ds,4),(Ds,4),(G,4),(E,4),(E,4),(A,4),(G,4),(Cs,4),(Gs,4),(F,4),(Cs,4),(Fs,4)]
r3_pitch = [(Fs,4),(F,4),(D,4),(Gs,4),(Fs,4),(Ds,4),(Fs,4),(A,4),(D,4),(E,4),(G,4),(Cs,4)]
r4_pitch = [(G,4),(Ds,4),(Ds,4),(A,4),(E,4),(Ds,4),(F,4),(Gs,4),(E,4),(E,4),(Fs,4),(Ds,4)]

-- muzyka
r1_play = line $ toMusicPitch hn (r1_pitch)
r2_play = line $ toMusicPitch qn (r2_pitch)
r3_play = line $ toMusicPitch hn (r3_pitch)
r4_play = line $ toMusicPitch qn (r4_pitch)

rotate_utwor = r1_play :+: r2_play :+: r3_play :+: r4_play

-- granie
test :: IO ()
test = do
    putStrLn "oryginalny utwor" 
    play utwor
    putStrLn "Ten sam utwor, ale po obrocie"
    play rotate_utwor



--funkcje 

zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

toInt:: [Double] -> [Int]
toInt as =  map round as

pcToInt12 :: PitchClass -> Int
pcToInt12 x = pcToInt x `mod` 12 

toAbsPitches :: [Pitch] -> [AbsPitch]
toAbsPitches ps = map absPitch ps
    
    
toPitches :: [AbsPitch] -> [Pitch]
toPitches as = map pitch  as

-- Konwersja listy int na float
floatList :: [AbsPitch] -> [Float]
floatList [] = []
floatList (n:ns)  = (x) : floatList(ns)
    where x = fromIntegral n :: Float

--Iloczyn Skalarny wektorow
dotProduct :: Vector -> Vector -> Double
dotProduct v w = sum (zipWith' (*) v w)
-- mnozenie macierzy
mmult :: Num a => [[a]] -> [[a]] -> [[a]]
mmult a b  = [[sum $ zipWith (*) ar bc | bc <- (L.transpose b)] | ar <- a]

toMusicPitch :: Dur ->[Pitch]->[Music Pitch]
toMusicPitch   _ [] = [rest qn]
toMusicPitch  i (w:ws) = note i w : toMusicPitch i ws

-- konwersja macierzy na wektor
conv :: Matrix -> Vector
conv = foldr (++) []

--dodanie nut

-- Definicja nuty (rownorzedny zapis)

--Prim (Note qn (C, 0))
--note qn (C, 4)
--c 4 qn

pcToSN :: PitchClass -> Music Pitch
pcToSN pc = note sn (pc, 4)

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

pcToHN :: PitchClass -> Music Pitch
pcToHN pc = note hn (pc, 4)
