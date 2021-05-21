import Euterpea
import Data.List

toInt::Float -> Int
toInt x = round x

rx t = [[1,0,0],[0, cos t,-sin t], [0, sin t,cos t]]
ry t = [[cos t, 0, sin t],[0,1,0], [-sin t,0 ,cos t]]
rz t = [[cos t, -sin t, 0],[sin t, cos t, 0], [0,0,1]]

rxyz t = a_x_w (a_x_b (a_x_b (rx t) (ry t))(rz t)) 
  
a=[[1,2,3],[4,5,6],[6,7,8]]
w :: [Float]
w=[62,65,69]
ww :: [Float]
ww =[42,49,56]

w_x_c [][]= 0
w_x_c (w:ws) (c:cs) = w * c + w_x_c ws cs

kolumna i a b = [ w_x_c (a!!i) (b!!j) | j<-[0..length b-1]]

--mnozeniemacierzy a i b wymiaru nxn
--a_x_b :: [[Int]] -> [[Int]]-> [[Int]]
a_x_b a b = [kolumna i a (Data.List.transpose b)| i<-[0..length a -1]]

--mnozenie macierzy przez wektor
--a_x_w :: [[Int]] -> [Int]-> [Int]
a_x_w a w = [ w_x_c (a!!i) w | i<-[0..length a-1]]

toMusicPitch :: Dur ->[Pitch]->[Music Pitch]
toMusicPitch   _ [] = [rest qn]
toMusicPitch  i (w:ws) = note i w : toMusicPitch i ws 

p w = map pitch (map toInt w)
m = line $ toMusicPitch qn (p w)

w1 t w = map pitch (map toInt (rxyz t w))
m1 t w = line $ toMusicPitch qn (w1 t w)

nowe_w t stare_w = rxyz t stare_w

--ciag floatow po obrocie
generuj :: Int->[Float]->[Float]
generuj 0 s = []
generuj n s = s ++ (generuj (n-1) w1)
    where
       w1 = (nowe_w (0.1) s)



w2 = map pitch (map toInt (generuj 20 w))
m2 = line $ toMusicPitch en w2
ww2 = map pitch (map toInt (generuj 5 ww))
mm2 = line $ toMusicPitch hn ww2
mmm = m2 :=: mm2
