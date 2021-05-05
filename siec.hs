import Euterpea
-- konwersja z Pitch na AbsPitch
toAbsPitch :: [Pitch] -> [AbsPitch]
toAbsPitch [] = []
toAbsPitch (x:xs) =  absPitch x : toAbsPitch xs

-- konwersja z AbsPitch na Pitch
toPitch :: [AbsPitch] -> [Pitch]
toPitch [] = []
toPitch (x:xs) = pitch x : toPitch xs

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

-- Obracanie listy

rotate :: Int -> [AbsPitch] -> [AbsPitch]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs -- n liczba wymiarow

-- 1 utwor: http://oskarkolberg.pl/en-US;q=0.7/MusicDb/Details/cafbb535-ed69-4b99-9002-f65fe40ed8fe
-- "Od Pilicy i Wolbroma"

takt1 = line [a 4 qn, rest sn, a 4 sn, a 4 sn, gs 4 sn,  e 4 sn]
takt2 = line [a 4 qn, rest qn, a 4 sn, a 4 sn, b 4 sn, gs 4 sn]
takt3 = line [a 4 sn, a 4 sn, c 4 qn, d 4 qn, e 4 hn]
takt4 = line [e 4 sn, e 4 sn, e 4 sn, e 4 sn,  d 4 sn, c 4 sn]
takt5 = line [b 4 sn,  d 4 sn,  d 4 sn, d 4 sn, d 4 sn, c 4 sn]
takt6 = line [a 4 sn, b 4 sn, c 4 qn, rest qn, c 4 qn, rest qn, a 4 hn]

--calosc:
utwor1 = takt1 :+: takt2 :+: takt3 :+: takt4 :+: takt5 :+: takt6

-- utwor jazzowy

jazz = line [a 5 en, f 5 en, g 5 en, f 5 en, a 5 qn, f 5 qn, a 5 qn, f 5 qn, g 5 qn, a 5 qn, f 5 qn, g 5 qn, f 5 qn]


-- Pitch 

linia1 = [(A,4), (A,4), (Gs,4), (E,4)]
linia2 = [(A,4), (A,4), (A,4), (B,4), (Gs,4)]
linia3 = [(A,4), (A,4), (C,4), (D,4), (E,4)]
linia4 = [(E,4), (E,4), (E,4), (E,4), (D,4), (A,4)]
linia5 = [(B,4), (D,4), (D,4), (D,4), (D,4), (C,4)]
linia6 = [(A,4), (B,4), (C,4), (C,4), (A,4)]

-- konwersja na AbsPitch:

abs1 = toAbsPitch linia1
abs2 = toAbsPitch linia2
abs3 = toAbsPitch linia3
abs4 = toAbsPitch linia4
abs5 = toAbsPitch linia5
abs6 = toAbsPitch linia6

-- laczenie w calosc

caly = concat [abs1, abs2, abs3, abs4, abs5, abs6]

rotate1 = rotate 3 caly -- obrot macierzy w 3 wymiarach
rotate2 = rotate 4 caly -- -- obrot macierzy w 4 wymiarach

-- utwor po konwersji
rotate3 = rotate1 ++ rotate2
new = toPitch rotate3



oktawa1 =  [(E,4),(A,4),(A,4),(A,4)]
oktawa2 = [(B,4),(Gs,4),(A,4),(A,4),(C,4)]
oktawa3 = [(D,4),(E,4),(E,4),(E,4),(E,4)]
oktawa4 = [(E,4),(D,4),(A,4),(B,4),(D,4),(D,4)]
oktawa5 = [(D,4),(D,4),(C,4),(A,4),(B,4),(C,4)]
oktawa6 = [(C,4),(A,4),(A,4),(A,4),(Gs,4)]

-- dodanie nut  po :+: pauza
utwor2 =
    let m1 = line (map pcToQN [E,A,A,A]) 
        m2 = line (map pcToQN [B,Gs,A,A,C]) 
        m3 = line (map pcToQN [D,E,E,E])
        m4 = line (map pcToQN [E,D,A,B,D,D])
        m5 = line (map pcToQN [D,D,C,A,B,C])
        m6 = line (map pcToQN [C,A,A,A,Gs])
        
    in line [m1, m2, m3]

  -- granie

  -- Stary utwor:
stary = utwor1
--nowy
nowy = utwor2



    
