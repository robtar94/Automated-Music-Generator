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
-- Dane
-- v1 do v4 to akordy
v1 :: Vector
v1 = [60,64,67]
v2 :: Vector
v2 = [69,61,64]
v3 :: Vector
v3 = [67,71,62]
v4 :: Vector
v4 = [65,69,60]
-- wektory upakowane w macierze
w1 :: Matrix
w1 = [v1]
w2 :: Matrix
w2 = [v2]
w3 :: Matrix
w3 = [v3]

w4 :: Matrix
w4 = [v4]


-- macierze do obrotu 3D
mXY = mmult (mx 5) (my 5) 
mXYZ = mmult (mXY) (mz 5) -- cala macierz obrotu o kat 5

--obracanie wektorow
wektor1 = do
    toInt(concat(mmult (w1) mXYZ))
    let nw1 = [80,37,66]
    map pitch nw1

wektor2 = do
    toInt(concat(mmult (w2) mXYZ))
    let nw2 = [92,34,55]
    map pitch nw2

wektor3 = do
    toInt(concat(mmult (w3) mXYZ))
    let nw3 = [95,42,50]
    map pitch nw3

wektor4 = do
    toInt(concat(mmult (w4) mXYZ))
    let nw4 = [93,41,48]
    map pitch nw4

    
    
--obrocony wektor 1
rw1 :: [(PitchClass, Octave)]
rw1 = [(Gs,5),(Cs,2),(Fs,4),(G,5),(G,1), (Cs, 5)]
rw1_play :: Music Pitch
rw1_play =  chord $ toMusicPitch qn (rw1)

-- oryginalny wektor1
akord1 = [60,64,67]
akord1_pitch = map pitch akord1
akord1_music = chord $ toMusicPitch qn (akord1_pitch) -- oryginalny

--obrocony wektor 2
rw2 = [(Gs,6),(As,1),(G,3)]
rw2_play = chord $ toMusicPitch qn (rw2)

--oryginalny wektor 2
akord2 = [69,61,64]
akord2_pitch = map pitch akord2
akord2_music = chord $ toMusicPitch qn (akord2_pitch)

-- obrocony wektor3
rw3 = [(B,6),(Fs,2),(D,3)]
rw3_play = chord $ toMusicPitch qn (rw2)

--oryginalny wektor 3
akord3 = [67,71,62]
akord3_pitch = map pitch akord3
akord3_music = chord $ toMusicPitch qn (akord3_pitch)

--obrocony wektor 4
rw4 = [(A,6),(F,2),(C,3)]
rw4_play = chord $ toMusicPitch qn (rw4)

--oryginalny wektor 4
akord4 = [65,69,60]
akord4_pitch = map pitch akord4
akord4_music = chord $ toMusicPitch qn (akord4_pitch)



--granie

pierwsza_para :: Music Pitch
pierwsza_para = akord1_music :+: rw1_play

druga_para :: Music Pitch 
druga_para = akord2_music :+: rw2_play

trzecia_para :: Music Pitch 
trzecia_para = akord3_music :+: rw3_play

czwarta_para :: Music Pitch
czwarta_para = akord4_music :+: rw4_play

-- granie wszytskich par
test :: IO ()
test = do
    putStrLn "pierwsza para"
    play pierwsza_para
    putStrLn "Druga para"
    play druga_para
    putStrLn "Trzecia Para"
    play trzecia_para
    putStrLn "Czwarta para"
    play czwarta_para




  
 
    
    






 


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

--Prim (Note qn (C, 4))
--note qn (C, 4)
--c 4 qn

pcToSN :: PitchClass -> Music Pitch
pcToSN pc = note sn (pc, 4)

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

pcToHN :: PitchClass -> Music Pitch
pcToHN pc = note hn (pc, 4)

