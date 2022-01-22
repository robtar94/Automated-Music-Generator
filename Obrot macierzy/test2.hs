import Euterpea
import Data.List as L
import qualified Data.Vector.Unboxed as U


-- definicja typów
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
type Vector = [Double]
type Matrix = [[Double]]

-- macierz obrotu
mx p = [[1,0,0], [0, cos p, sin p], [0, -sin p, cos p]] -- p = phi
my p = [[cos p, 0, - sin p], [0, 1, 0], [sin p, 0, cos p]]
mz p = [[cos p, sin p, 0], [-sin p, cos p, 0], [0, 0, 1]]

-- gama c-dur
gamaC :: [Pitch]
gamaC = [(C, 4), (D, 4), (E, 4), (F, 4), (A, 4), (B, 4), (C,4)]
absGamaC :: [AbsPitch]
absGamaC = [60,62,64,65,69,71,60]

-- akordy
cDur :: [Pitch]
cDur = [(C, 4), (E, 4), (G, 4)]
aDur :: [Pitch]
aDur = [(A, 4), (Cs, 4), (E, 4)]
gDur :: [Pitch]
gDur = [(G, 4), (B, 4), (D, 4)]
fDur :: [Pitch]
fDur = [(F, 4), (A, 4), (C, 4)]

-- akordyAbs
abscDur :: [AbsPitch]
abscDur = [60,64,67]
absaDur :: [AbsPitch]
absaDur = [69,61,64]
absGdur :: [AbsPitch]
absGdur = [67,71,62]
absFdur :: [AbsPitch]
absFdur = [65,69,60]


-- wektory
-- v1 do v4 to akordy
v1 :: Vector
v1 = [60,64,67]
v2 :: Vector
v2 = [69,61,64]
v3 :: Vector
v3 = [67,71,62]
v4 :: Vector
v4 = [65,69,60]
-- U - to gama C-dur
u :: Vector
u = [60,62,64,65,69,71,60]

-- Obliczenia

-- 1. Matryca wektorow
akordy :: Matrix
akordy = [[60,64,67], 
          [69,61,64],
          [67,71,62],
          [65,69,60]]
gama :: Matrix
gama = [u]

-- obliczenia
t1 :: Matrix
t1 = [u,v2]



f = [[1,0,0]]

test = toInt(concat(mmult Main.f (mz 90)))





-- funkcje

--ZipWith'
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

toInt:: [Double] -> [Int]
toInt as =  map round as

toAbsPitches :: [Pitch] -> [AbsPitch]
toAbsPitches ps = map absPitch ps
    
    
toPitches :: [AbsPitch] -> [Pitch]
toPitches as = map pitch as

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
mmult a b = [[sum $ zipWith (*) ar bc | bc <- (L.transpose b)] | ar <- a]

toMusicPitch :: Dur ->[Pitch]->[Music Pitch]
toMusicPitch   _ [] = [rest qn]
toMusicPitch  i (w:ws) = note i w : toMusicPitch i ws

-- konwersja macierzy na wektor
conv :: Matrix -> Vector
conv = foldr (++) []